#define NS_PROTOCOL "tcp_compound.c"
/*
 *   TCP Compound.
 *	Source maintained at <http://netlab.caltech.edu/lachlan/ctcp>
 *
 *   further details can be found here:
 *      ftp://ftp.research.microsoft.com/pub/tr/TR-2005-86.pdf
 *   earlier release, requested to go in 2.6.18:
 *	http://lwn.net/Articles/185074/
 *
 * Release 1.3
 * Apr 2008:  Fix overflow bug (old_wnd*brtt) for large BDP -- LA
 * Apr 2008:  Add pacing of  dwnd  increase -- LA
 * Apr 2008:  Port to NS2 TCP-Linux -- LA
 * Release 1.2
 * Mar 2008:  Call  tcp_reno_cong_avoid()  to support  abc  -- LA
 * Mar 2008:  Add "#if LINUX_VERSION_CODE" to support APIs, from 2.6.14 on -- LA
 * Release 1.1
 * Mar 2008:  LAMBDA = 1/4 (was 1/2) -- LA
 * Mar 2008:  Option for DWND measured in bytes, not packets. -- LA
 * Mar 2008:  Option for (dwnd)^0.75  not  (wnd)^0.75 (old i'net drafts) -- LA
 * Mar 2008:  Introduce LOW_WINDOW -- LA
 * Feb 2008:  Replace Newton-Raphson calculation with  dwnd/sqrt(sqrt(dwnd)) -LA
 * Jan 2008:  Module parameters added -- LA
 * Dec 2007:  Gamma tuning added -- LA
 * Nov 2007:  Disable reset of baseRTT in  tcp_compound_cwnd_event() -- LA
 * Nov 2007:  Port to 2.6.23 -- Lachlan Andrew
 * May 2006:  Original release -- Angelo P. Castellani, Stephen Hemminger
 */

//#define TRANSPORT_DEBUG 1

/* INFOCOM VERSION used (wnd)^0.75,   draft-00 and -01 uses (dwnd)^0.75 */
/* (Note: non-INFOCOM_VERSION is deprecated and currently has bugs) */
#define INFOCOM_VERSION
#ifdef INFOCOM_VERSION
#  define DWND_IN_PACKETS
#endif

#ifdef __KERNEL__
#  include <linux/mm.h>
#  include <linux/module.h>
#  include <linux/skbuff.h>
#  include <linux/inet_diag.h>
#  include <net/tcp.h>
#  include <linux/version.h>
#else
/* If not compiling for the kernel, assume it is for NS2 TCP-Linux */
#  include "../ns-linux-c.h"
#  include "../ns-linux-util.h"
#endif

/* Default to 2.6.22 if not specified... (Used by TCP-Linux?) */
#ifndef LINUX_VERSION_CODE
#  define LINUX_VERSION_CODE    (((2) << 16) + ((6) << 8) +(22))
#  define KERNEL_VERSION(a,b,c) (((a) << 16) + ((b) << 8) + (c))
#endif

/* If additional monitoring not enabled, just ignore WEB100_VAR_SET */
#ifndef LACHLAN_WEB100
#  define WEB100_VAR_SET(tp, var, val)
#endif

#ifdef TRANSPORT_DEBUG
#  define dbg_print(a) printk a
#else
#  define dbg_print(a) 
#endif

/* Default values of the CTCP variables */

/* Fixed point, used by  diff_reno, diff and target_cwnd */
#define C_PARAM_SHIFT 1

static unsigned int LOG_ALPHA __read_mostly = 3U;	/* alpha = 1/8 */
static unsigned int ETA       __read_mostly = 1U;
static int GAMMA              __read_mostly = 30;
static int GAMMA_LOW          __read_mostly = 5;
static int GAMMA_HIGH         __read_mostly = 30;
static int LAMBDA_SHIFT       __read_mostly = 2;	/* lambda = 1/4 */
static int LOW_WINDOW         __read_mostly = 38;	/* dwnd only used if cwnd>LOW_WINDOW  */
static int PACE_DWND          __read_mostly = 1;	/* spread out increases */

module_param(    LOG_ALPHA, int, 0644);
MODULE_PARM_DESC(LOG_ALPHA, "Log_2 of factor of WND^0.75 by which DWND increases");
module_param(    ETA,  int, 0644);
MODULE_PARM_DESC(ETA,  "Factor of estimated queue by which DWND decreases");
module_param(    GAMMA, int, 0644);
MODULE_PARM_DESC(GAMMA, "Delay threshold (pks)");
module_param(    GAMMA_LOW, int, 0644);
MODULE_PARM_DESC(GAMMA_LOW, "GAMMA tuning: min value possible");
module_param(    GAMMA_HIGH, int, 0644);
MODULE_PARM_DESC(GAMMA_HIGH, "GAMMA tuning: max value possible");
module_param(    LAMBDA_SHIFT, int, 0644);
MODULE_PARM_DESC(LAMBDA_SHIFT, "GAMMA tuning: log_2(Forgetting factor)");
module_param(    LOW_WINDOW, int, 0644);
MODULE_PARM_DESC(LOW_WINDOW, "Revert to Reno if CWND < LOW_WINDOW");
module_param(    PACE_DWND, int, 0644);
MODULE_PARM_DESC(PACE_DWND,"increase DWND smoothly over one RTT, not in jumps");

/* TCP compound variables */
struct compound {
	u32 beg_snd_nxt;	/* right edge during last RTT */
	u32 beg_snd_una;	/* left  edge during last RTT */
	u32 beg_cwnd;		/* saves the size of cwnd only (gamma tuning) */
	u8  doing_ctcp_now;	/* if true, do ctcp for this RTT */
	u16 cntRTT;		/* # of RTTs measured within last RTT */
	u32 minRTT;		/* min of RTTs measured within last RTT (in usec) */
	u32 baseRTT;		/* the min of all CTCP RTT measurements seen (in usec) */

	u32 cwnd;
	u32 dwnd;
	s32 diff_reno;		/* used for gamma-tuning. << C_PARAM_SHIFT */
							/* -1 = invalid */
	u16 gamma;		/* target packets to store in the network */

	/* pacing of increments of DWND */
	u32 incr_interval;	/* dwnd++ once per incr_interval ACKS */
	u32 dwnd_left;		/* number of increments not yet done this RTT */
	u32 ack_since_dwnd_inc;
};

/* There are several situations when we must "re-start" CTCP:
 *
 *  o when a connection is established
 *  o after an RTO
 *  o after fast recovery
 *  o when we send a packet and there is no outstanding
 *    unacknowledged data (restarting an idle connection)
 *
 * In these circumstances we cannot do a CTCP calculation at the
 * end of the first RTT, because any calculation we do is using
 * stale info -- both the saved cwnd and congestion feedback are
 * stale.
 *
 * Instead we must wait until the completion of an RTT during
 * which we actually receive ACKs.
 *
 * FIXME: After an RTO, baseRTT *should* be reset.  Where?
 */
static inline void ctcp_enable(struct sock *sk)
{
	const struct tcp_sock *tp = tcp_sk(sk);
	struct compound *ctcp = inet_csk_ca(sk);

	/* Begin taking CTCP samples next time we send something. */
	ctcp->doing_ctcp_now = 1;

	/* Set the beginning of the next send window. */
	ctcp->beg_snd_nxt = tp->snd_nxt;

	ctcp->cntRTT = 0;
	ctcp->minRTT = 0x7fffffff;
	ctcp->diff_reno = -1;		/* set to invalid */
	
	/* Reset any paced dwnd increases */
	/* (Causes paced increases to be lost after a state change) */
	ctcp->incr_interval = 0x7fffffff;
	ctcp->dwnd_left = 0;
	ctcp->ack_since_dwnd_inc = 0;
}

/******************************************************************************/
/* Stop taking CTCP samples for now.  */
/* Called when not in OPEN state (e.g. in LOSS). */
static inline void ctcp_disable(struct sock *sk)
{
	struct compound *ctcp = inet_csk_ca(sk);

	ctcp->doing_ctcp_now = 0;
}

/******************************************************************************/
/* Initialize a connection.  Not called when restarted after timeout etc */
static void tcp_compound_init(struct sock *sk)
{
	struct compound *ctcp = inet_csk_ca(sk);
	const struct tcp_sock *tp = tcp_sk(sk);

	ctcp->baseRTT = 0x7fffffff;
	ctcp_enable(sk);

	ctcp->dwnd = 0;
	ctcp->cwnd = tp->snd_cwnd;

	ctcp->gamma = GAMMA;
	ctcp->diff_reno = -1;		/* set to invalid */

}

/******************************************************************************/
/* slow start threshold < cwnd/2 like tcp_reno_ssthresh, */
/* but also do gamma tuning */
/* (The reduction of  dwnd  is done in  ctcp_cong_avoid()  by reducing */
/* both  cnwd  and  dwnd  to match a reduced   tp->snd_cwnd) */
u32 tcp_compound_ssthresh(struct sock *sk)
{
	struct compound *ctcp = inet_csk_ca(sk);
	const struct tcp_sock *tp = tcp_sk(sk);

	/* If  diff_reno  valid, do gamma tuning */
	if (ctcp->diff_reno != -1) {
		/* g_sample = diff_reno * 3/4 */
		u32 g_sample = (ctcp->diff_reno * 3) >> (C_PARAM_SHIFT+2);
		ctcp->gamma += ((s32)(g_sample - ctcp->gamma)) >> LAMBDA_SHIFT;
		dbg_print(("g_sample %d gamma %d ", g_sample, ctcp->gamma));

		/* clip */
		if (ctcp->gamma > GAMMA_HIGH)
		    ctcp->gamma = GAMMA_HIGH;
		else if (ctcp->gamma < GAMMA_LOW)
			 ctcp->gamma = GAMMA_LOW;
		/* after an adjustment, wait for fresh  diff_reno  measurement
		 * before making the next one*/
		ctcp->diff_reno = -1;
	}
	dbg_print(("CTCP %p ssthresh %d\n", tp, tp->snd_cwnd >> 1U));

	/* halve window on loss */
	return max(tp->snd_cwnd >> 1U, 2U);
}

/******************************************************************************/
/* Do RTT sampling needed for CTCP.
 * Basically we:
 *   o min-filter RTT samples from within an RTT to get the current
 *     propagation delay + queuing delay (we are min-filtering to try to
 *     avoid the effects of delayed ACKs)
 *   o min-filter RTT samples from a much longer window (forever for now)
 *     to find the propagation delay (baseRTT)
 *
 * Should also deal with active drop under Early Congestion Indication.
 */
#if    LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,23)
static void tcp_compound_pkts_acked(struct sock *sk, u32 num_acked, s32 rtt_us)
#elif  LINUX_VERSION_CODE == KERNEL_VERSION(2,6,22)
static void tcp_compound_pkts_acked(struct sock *sk, u32 num_acked, ktime_t lst)
#else
static void tcp_compound_rtt_sample(struct sock *sk, u32 rtt_us)
#endif
{
	struct compound *ctcp = inet_csk_ca(sk);
	struct tcp_sock *tp = tcp_sk(sk);
	u32 vrtt;

	/* ignore dubious RTT measurement */
#if LINUX_VERSION_CODE == KERNEL_VERSION(2,6,22)
	s32 rtt_us = ktime_to_us(net_timedelta(lst));
	if (ktime_equal(lst, net_invalid_timestamp()))
#else
	if (rtt_us < 0)
#endif
		return;

	/* Never allow zero rtt or baseRTT */
	vrtt = rtt_us + 1;

	/* Filter to find propagation delay: */
	if (vrtt < ctcp->baseRTT)
	{
		ctcp->baseRTT = vrtt;
		WEB100_VAR_SET(tp, BaseRTT, ctcp->baseRTT);
	}	

	/* Find the min RTT during the last RTT to find
	 * the current prop. delay + queuing delay:
	 */

	ctcp->minRTT = min(ctcp->minRTT, vrtt);
	ctcp->cntRTT++;
}

/******************************************************************************/
static void tcp_compound_state(struct sock *sk, u8 ca_state)
{

	if (ca_state == TCP_CA_Open)
		ctcp_enable(sk);
	else
		ctcp_disable(sk);
}


#if 0
/* 64bit divisor, dividend and result. dynamic precision */
static inline u64 div64_64_optimized(u64 dividend, u64 divisor)
{
	u32 d = divisor;

	if (divisor > 0xffffffffULL) {
		unsigned int shift = fls(divisor >> 32);

		d = divisor >> shift;
		dividend >>= shift;
	}

	/* avoid 64 bit division if possible */
	if (dividend >> 32)
		do_div(dividend, d);
	else
		dividend = (u32) dividend / d;

	return dividend;
}

/* calculate the quartic root of "a" using Newton-Raphson */
static u32 qroot(u64 a)
{
	u32 x, x1;

	/* Initial estimate is based on:
	 * qrt(x) = exp(log(x) / 4)
	 */
	x = 1u << (fls64(a) >> 2);

	/*
	 * Iteration based on:
	 *                         3
	 * x    = ( 3 * x  +  a / x  ) / 4
	 *  k+1          k         k
	 */
	do {
		u64 x3 = x;

		x1 = x;
		x3 *= x;
		x3 *= x;

		x = (3 * x + (u32) div64_64_optimized(a, x3)) / 4;
	} while (abs(x1 - x) > 1);

	return x;
}
#endif

#ifndef __KERNEL__
/* For NS2 TCP-Linux, provide the integer square root code */
static unsigned long int_sqrt(unsigned long x)
{
	unsigned long op, res, one;
	int BITS_PER_LONG = 8*sizeof(long);

	op = x;
	res = 0;

	one = 1UL << (BITS_PER_LONG - 2);
	while (one > op)
		one >>= 2;

	while (one != 0) {
		if (op >= res + one) {
			op = op - (res + one);
			res = res +  2 * one;
		}
		res /= 2;
		one /= 4;
	}
	return res;
}
#endif

/******************************************************************************/
/*
 * If the connection is idle and we are restarting,
 * then we don't want to do any CTCP calculations
 * until we get fresh RTT samples.  So when we
 * restart, we reset our CTCP state to a clean
 * slate. After we get acks for this flight of
 * packets, _then_ we can make CTCP calculations
 * again.
 */
static void tcp_compound_cwnd_event(struct sock *sk, enum tcp_ca_event event)
{
	if (event == CA_EVENT_CWND_RESTART || event == CA_EVENT_TX_START)
	{
		struct compound *ctcp = inet_csk_ca(sk);
		int tmp = ctcp->baseRTT;	/* CTCP spec keeps baseRTT */
		tcp_compound_init(sk);
		//if (event == CA_EVENT_CWND_RESTART)
		ctcp->baseRTT = tmp;
	}
}

/******************************************************************************/
/******************************************************************************/
#if   LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,25)
static void tcp_compound_cong_avoid(struct sock *sk, u32 ack, u32 in_flight)
#elif LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,23)
static void tcp_compound_cong_avoid(struct sock *sk, u32 ack,
				    u32 in_flight, int flag)
#else
static void tcp_compound_cong_avoid(struct sock *sk, u32 ack, u32 seq_rtt,
				    u32 in_flight, int flag)
#endif
/**************************************/
{
	struct tcp_sock *tp = tcp_sk(sk);
	struct compound *ctcp = inet_csk_ca(sk);
	//u8 inc = 0;

#ifdef DWND_IN_PACKETS
	int dwnd_unit = 1;
#else
	int dwnd_unit = tp->mss_cache;
#endif
	__u32 prev_total_window = ctcp->dwnd + dwnd_unit * ctcp->cwnd;
	/* Update  ctcp->cwnd  to reflect external decreases in  snd_cwnd */
	/* Does this also reflect the "beta" change on loss?? -- LA 16-Jan-08 */
	/* Yes -- LA 12-Mar-08 */
	if (prev_total_window > tp->snd_cwnd * dwnd_unit) {
		/* on backoff, reduce cwnd and dwnd in the same proportion */
		/* dwnd <- prev_dwnd* (snd_cwnd[d_unit] / prev_total[d_unit]) */
		/* but multiply first to avoid truncation */
		__u64  numerator = ctcp->dwnd * tp->snd_cwnd * dwnd_unit;
		dbg_print (("CTCP %p Backoff cwnd %u + %u -> %u",
			tp, ctcp->cwnd, ctcp->dwnd, tp->snd_cwnd));
		do_div(numerator,prev_total_window);
		ctcp->dwnd = numerator;
		ctcp->cwnd = tp->snd_cwnd - (ctcp->dwnd / dwnd_unit);
		/*
		if (ctcp->cwnd > tp->snd_cwnd || ctcp->dwnd > tp->snd_cwnd) {
			ctcp->cwnd = tp->snd_cwnd;
			ctcp->dwnd = 0;
		} else
			ctcp->cwnd = tp->snd_cwnd - ctcp->dwnd;
		*/
		dbg_print(("= %u + %u\n", ctcp->cwnd, ctcp->dwnd));
	}
	else if (prev_total_window < tp->snd_cwnd * dwnd_unit) {
		dbg_print(("CTCP %p prev_total %d < snd_cwnd %d\n",
			tp, prev_total_window, tp->snd_cwnd * dwnd_unit));
	}

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,15)
	if (!tcp_is_cwnd_limited(sk, in_flight)) {
		/*
		dbg_print(("CTCP %p not cwnd limited: cwnd %d in_flight %d\n",
				tp, tp->snd_cwnd,in_flight));
		*/
		return;
	}
#endif

	/* Why does this not call  tcp_slow_start(tp)  and/or consider  abc? */
	/* Is it because it increases  ctcp->cwnd  instead of  tp->snd_cwnd? */
	/* Perhaps:
	   in slow start, call slow_start()  and set  ctcp->cwnd=snd_cwnd - dwdn
	   otherwise, do  abc  stuff to  ctcp->cwnd
	   */
#if 0
	if (ctcp->cwnd <= tp->snd_ssthresh)
		inc = 1;
	else if (tp->snd_cwnd_cnt < tp->snd_cwnd)
		tp->snd_cwnd_cnt++;

	if (tp->snd_cwnd_cnt >= tp->snd_cwnd) {
		inc = 1;
		tp->snd_cwnd_cnt = 0;
	}

	if (inc && tp->snd_cwnd < tp->snd_cwnd_clamp)
		ctcp->cwnd++;
#else
#  if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,25)
	tcp_reno_cong_avoid(sk, ack, in_flight);
#  elif LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,23)
	tcp_reno_cong_avoid(sk, ack, in_flight, flag);
#  else
	tcp_reno_cong_avoid(sk, ack, seq_rtt, in_flight, flag);
#  endif

	ctcp->cwnd = tp->snd_cwnd - ctcp->dwnd/dwnd_unit;
#endif


	/* Pace out increase in  dwnd, once per  incr_interval */
	/* Doesn't use ABC */
	if (ctcp->dwnd_left > 0) {
		if (++(ctcp->ack_since_dwnd_inc) > ctcp->incr_interval)
		{
			ctcp->ack_since_dwnd_inc = 0;
			ctcp->dwnd_left--;
			ctcp->dwnd += dwnd_unit;
		}
	}


	/* Once per RTT, caluclate change in dwnd */
	/* FIXME (This won't be triggered during long SACK events.  Problem?) */
	if (after(ack, ctcp->beg_snd_nxt)) {
		u32 old_wnd, old_cwnd;

		if (!tp->mss_cache)
			return;

		/* Finish last RTT's paced incrementing, if paced too slowly */
		ctcp->dwnd += ctcp->dwnd_left;

		/* Here old_wnd is essentially the window of data that was
		 * sent during the previous RTT, and has all
		 * been acknowledged in the course of the RTT that ended
		 * with the ACK we just received. Likewise, old_snd_cwnd
		 * is the cwnd during the previous RTT.
		 */
		old_wnd = (ctcp->beg_snd_nxt - ctcp->beg_snd_una) /
		    tp->mss_cache;
		old_cwnd = ctcp->beg_cwnd;

		/* Save the extent of the current window so we can use this
		 * at the end of the next RTT.
		 */
		ctcp->beg_snd_una = ctcp->beg_snd_nxt;
		ctcp->beg_snd_nxt = tp->snd_nxt;
		ctcp->beg_cwnd = ctcp->cwnd;


		/* CTCP is disabled for small cwnds, the case Reno handles OK */
		if (tp->snd_cwnd < LOW_WINDOW || tp->snd_cwnd< tp->snd_ssthresh)
		{
			if (ctcp->dwnd)
				dbg_print(("CTCP %p: Low window: %d %d\n",
					tp, tp->snd_cwnd, ctcp->dwnd));
			ctcp->dwnd = 0;
		} else
		/* We do the DWND calculations only if we got enough RTT
		 * samples that we can be reasonably sure that we got
		 * at least one RTT sample that wasn't from a delayed ACK.
		 * If we only had 2 samples total,
		 * then that means we're getting only 1 ACK per RTT, which
		 * means they're almost certainly delayed ACKs.
		 * If  we have 3 samples, we should be OK.
		 */
		/* (Already updated CWND above, so ignore case cntRTT <= 2) */
		if (ctcp->cntRTT > 2) {
			u32 rtt, dwnd;
			u32 diff; 		/* shifted by C_PARAM_SHIFT */
			u64 target_cwnd;	/* shifted by C_PARAM_SHIFT */
			u64 brtt;

			/* We have enough RTT samples, so, using the CTCP
			 * algorithm, we determine if we should increase or
			 * decrease dwnd, and by how much.
			 */

			/* Pluck out the RTT we are using for the CTCP
			 * calculations. This is the min RTT seen during the
			 * last RTT. Taking the min filters out the effects
			 * of delayed ACKs, at the cost of noticing congestion
			 * a bit later.
			 */
			rtt = ctcp->minRTT;
			WEB100_VAR_SET(tp, EstQ, rtt - ctcp->baseRTT);

			/* Calculate the cwnd we should have, if we weren't
			 * going too fast.
			 *
			 * This is:
			 *     (actual rate in segments) * baseRTT
			 * We keep it as a fixed point number with
			 * C_PARAM_SHIFT bits to the right of the binary point.
			 */
			if (!rtt)
				return;

			brtt = ctcp->baseRTT;
			target_cwnd = (old_wnd * brtt) << C_PARAM_SHIFT;
			do_div(target_cwnd, rtt);

			/* Calculate the difference between the window we had,
			 * and the window we would like to have. This quantity
			 * is the "Diff" from the Arizona Vegas papers.
			 *
			 * Again, this is a fixed point number with
			 * C_PARAM_SHIFT bits to the right of the binary
			 * point.
			 */

			diff = (old_wnd << C_PARAM_SHIFT) - target_cwnd;
			/* Hacky re-use of AI and MD */
			WEB100_VAR_SET(tp, CurAI, diff);

			/* Analogously find "diff_reno" for gamma tuning */
			/* This time, use   old_cwnd   instead of  old_wnd */
			target_cwnd = (old_cwnd* brtt) << C_PARAM_SHIFT;
			do_div(target_cwnd, rtt);
			ctcp->diff_reno =
				(old_cwnd << C_PARAM_SHIFT) - target_cwnd;

			dwnd = ctcp->dwnd;

			dbg_print(("CTCP %p %lu diff %d g %d c %d d %d ",
					tp, jiffies, diff>>C_PARAM_SHIFT,
					ctcp->gamma, ctcp->cwnd, ctcp->dwnd));
			if (diff < (ctcp->gamma << C_PARAM_SHIFT)) {
				u64 v;
				u32 x;

				/*
				 * The TCP Compound paper describes the choice
				 * of "k" determines the agressiveness,
				 * ie. slope of the response function.
				 *
				 * For same value as HSTCP would be 0.8
				 * but for computaional reasons, both the
				 * original authors and this implementation
				 * use 0.75.
				 */
				#ifdef INFOCOM_VERSION
				v = old_wnd * dwnd_unit;
				#else /* ndef INFOCOM_VERSION */
				v = dwnd;
				if (v == 0)
					/* needs DWND_IN_PACKETS undef */
					/* (else >>LOG_ALPHA makes incr 0) */
					v = dwnd_unit;
				#endif	/* INFOCOM_VERSION */

				/* x = qroot(v * v * v) >> LOG_ALPHA; */
				/* x = (v/int_sqt(int_sqrt(v))) >> LOG_ALPHA;*/

				v <<= 8;
				x = int_sqrt(int_sqrt(old_wnd)<<16);
				do_div(v,x);    /* v = pow(dwnd, 0.75) */
				x = v >> LOG_ALPHA;

				/* Compensate for CWND increase */
				if (x > 0)
					dwnd = x - 1;
				else
					dwnd = 0;

				dbg_print(("++++ %lu",
					(unsigned long int)dwnd));
				if (PACE_DWND && dwnd != 0) {
				    ctcp->dwnd_left = dwnd;
				    /* factor of 2 for delayed ACKs */
				    ctcp->incr_interval = tp->snd_cwnd/(2*dwnd);
				    if (!ctcp->incr_interval)
					ctcp->incr_interval++;
				    ctcp->ack_since_dwnd_inc=0;

				    dbg_print((" pace %d at %d ",
					ctcp->dwnd_left, ctcp->incr_interval));
				} else {
				    ctcp->dwnd += dwnd;
				}

			/* Reduce dwnd by  eta * "window diff" */
			/* Recall here diff = "window diff" << C_PARAM_SHIFT */
			} else {
				dbg_print(("---- %lu", (unsigned long int)
					    ((diff * ETA) >> C_PARAM_SHIFT)));
				if ((dwnd << C_PARAM_SHIFT) < (diff * ETA))
					dwnd = 0;
				else
					dwnd -= (diff * ETA) >> C_PARAM_SHIFT;
				ctcp->dwnd = dwnd;
				ctcp->dwnd_left = 0;	/* no incr during RTT */
			}

			dbg_print((" to %lu %lu\n",
				(unsigned long int)ctcp->dwnd,
				(unsigned long int)(ctcp->dwnd+ctcp->cwnd)));

		}
		else
			dbg_print(("CTCP count %d < 2\n", ctcp->cntRTT));

		/* Wipe the slate clean for the next RTT. */
		ctcp->cntRTT = 0;
		ctcp->minRTT = 0x7fffffff;
	}

	tp->snd_cwnd = ctcp->cwnd + ctcp->dwnd / dwnd_unit;

	/* Hacky re-use of AI and MD */
	WEB100_VAR_SET(tp, CurMD, ctcp->dwnd);
}

/******************************************************************************/
/* Extract info for TCP socket info provided via netlink. */
/* (Reuse interface from Vegas.) */
static void tcp_compound_get_info(struct sock *sk, u32 ext, struct sk_buff *skb)
{
	const struct compound *ca = inet_csk_ca(sk);
	if (ext & (1 << (INET_DIAG_VEGASINFO - 1))) {
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,22)
		struct tcpvegas_info info = {
			.tcpv_enabled = ca->doing_ctcp_now,
			.tcpv_rttcnt = ca->cntRTT,
			.tcpv_rtt = ca->baseRTT,
			.tcpv_minrtt = ca->minRTT,
		};

		nla_put(skb, INET_DIAG_VEGASINFO, sizeof(info), &info);

#else
		struct tcpvegas_info *info;

		info = RTA_DATA(__RTA_PUT(skb, INET_DIAG_VEGASINFO,
					  sizeof(*info)));

		info->tcpv_enabled = ca->doing_ctcp_now;
		info->tcpv_rttcnt = ca->cntRTT;
		info->tcpv_rtt = ca->baseRTT;
		info->tcpv_minrtt = ca->minRTT;
	rtattr_failure: ;
#endif

	}
}

/******************************************************************************/
static struct tcp_congestion_ops tcp_compound = {
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,22)
	.flags		= TCP_CONG_RTT_STAMP,
	.pkts_acked	= tcp_compound_pkts_acked,
#else
	.rtt_sample	= tcp_compound_rtt_sample,
#endif
	.init		= tcp_compound_init,
	.ssthresh	= tcp_compound_ssthresh,
	.min_cwnd       = tcp_reno_min_cwnd,
	.cong_avoid	= tcp_compound_cong_avoid,
	.set_state	= tcp_compound_state,
	.cwnd_event	= tcp_compound_cwnd_event,
	.get_info	= tcp_compound_get_info,

	.owner		= THIS_MODULE,
	.name		= "compound",
};

static int __init tcp_compound_register(void)
{
	BUG_ON(sizeof(struct compound) > ICSK_CA_PRIV_SIZE);
	tcp_register_congestion_control(&tcp_compound);
	return 0;
}

static void __exit tcp_compound_unregister(void)
{
	tcp_unregister_congestion_control(&tcp_compound);
}

module_init(tcp_compound_register);
module_exit(tcp_compound_unregister);

MODULE_AUTHOR("Lachlan Andrew, Angelo P. Castellani, Stephen Hemminger");
MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("TCP Compound");
MODULE_VERSION("1.3");
#undef NS_PROTOCOL
