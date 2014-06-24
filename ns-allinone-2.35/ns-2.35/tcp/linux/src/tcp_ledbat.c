#define NS_PROTOCOL "tcp_ledbat.c"
#include "../ns-linux-c.h"
#include "../ns-linux-util.h"
#include <stdlib.h>
//#include <malloc.h>
#include <assert.h>

/*
 * TCP-LEDBAT
 *
 * This code implements the congestion control algorithm described in
 * IETF DRAFT available at 
 * http://tools.ietf.org/html/draft-shalunov-ledbat-congestion-00
 * Notice that this code implements version 0 of the IETF draft.
 *
 * It is heavily inspired by the TCP-LP implementation (cfr. tcp_lp.c).
 *
 * Developed at TELECOM ParisTech by Silvio Valenti, (with some help 
 * by Claudio Testa and Dario Rossi)
 * 
 * If you use this code, please cite it as:
 * 
 * Dario Rossi, Claudio Testa, Silvio Valenti, Luca Muscariello, 
 * "LEDBAT: the new BitTorrent congestion control protocol", 
 * International Conference on Computer Communications and Networks 
 * (ICCCN 2010), Zurich, Switzerland, August, 2010.
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 *
 * Created by Silvio Valenti on Tue 2nd June 2009
 */

/* resolution of owd */
#define LP_RESOL       1000

/*rember that the len are the actual lenght - 1*/
static int base_histo_len   = 6;
static int noise_filter_len = 3;
static int target           = 25;
static int alpha1           = 1;
static int alpha2           = 1;
static int min_impl         = 0;
static int do_ss            = 0;
static int ledbat_ssthresh  = 65000;

module_param(base_histo_len, int, 0644);
MODULE_PARM_DESC(base_histo_len, "length of the base history vector");
module_param(noise_filter_len, int, 0644);
MODULE_PARM_DESC(noise_filter_len, "length of the noise_filter vector");
module_param(target, int, 0644);
MODULE_PARM_DESC(target, "target queuing delay");
module_param(alpha1, int, 0644);
MODULE_PARM_DESC(alpha1, "numerator of the gain");
module_param(alpha2, int, 0644);
MODULE_PARM_DESC(alpha2, "denominator of the gain");
module_param(min_impl, int, 0644);
MODULE_PARM_DESC(min_impl, "select which min: 0 base_histo, 1 global, 2 mov av");
module_param(do_ss, int, 0644);
MODULE_PARM_DESC(do_ss, "do slow start: 0 no, 1 yes, 2 with_ssthresh");
module_param(ledbat_ssthresh, int, 0644);
MODULE_PARM_DESC(ledbat_ssthresh, "slow start threshold");

struct owd_circ_buf {
	u16 *buffer;
	u8 first;
	u8 next;
	u8 len;
	u8 min;
};

/**
 * enum tcp_ledbat_state
 * @LEDBAT_VALID_RHZ: is remote HZ valid?
 * @LEDBAT_VALID_OWD: is OWD valid?
 *
 */
enum tcp_ledbat_state {
	LEDBAT_VALID_RHZ  = (1 << 0),
	LEDBAT_VALID_OWD  = (1 << 1),
	LEDBAT_CAN_SS     = (1 << 2),
};

enum ledbat_min_type {
	MIN_BASE_HISTO,
	MIN_GLOBAL,
	MIN_MAV,
};

/**
 * struct ledbat
 * @flag: TCP-LEDBAT state flag
 * @sowd: one-way delay moving average
 * @last_rollover: last minute timestamp
 * @remote_hz: estimated remote HZ
 * @remote_ref_time: remote reference time
 * @local_ref_time: local reference time
 * @base_history: circular buffer of the base history
 * @noise_filter: circular buffer of the noise filter
 * @target: current target
 * @next: next ledbat struct in the list of all open connections
 *
 */

struct ledbat {
	u32 last_rollover; //last minute time_stamp

	//var for delay calculation
	u32 remote_hz;
	u32 remote_ref_time;
	u32 local_ref_time;

	u32 snd_cwnd_cnt; //cwnd subcounter

	//base and noise filter history
	struct owd_circ_buf base_history;
	struct owd_circ_buf noise_filter;

	u32 flag; //status flag           
	u32 sowd; //min moving average
	int target; //current target
	struct ledbat *next; //pointer for list of ledbat structures

};

static struct ledbat *ledbat_head = NULL;
static int ledbat_open_sockets = 0;

static
int ledbat_init_circbuf(
	struct owd_circ_buf *buffer, 
	u16 len)
{
	u16 *b = malloc( len * sizeof(u16));
	if (b == NULL)
		return 1;
	//printk (KERN_INFO "size of ledbat_struct %d", sizeof(struct ledbat));
	buffer->len = len;
	buffer->buffer = b;
	buffer->first = 0;
	buffer->next = 0;
	buffer->min = 0;
	return 0;
}

static void tcp_ledbat_release(struct sock *sk)
{
	struct ledbat *ledbat = inet_csk_ca(sk);
	struct ledbat *p;

	free(ledbat->noise_filter.buffer);
	free(ledbat->base_history.buffer);

	if (!ledbat_head) 
	{
		printk ("AHHHHHHHHHH ledbat_head == NULL!!!%s", "\n");
	}

	if (ledbat_head == ledbat)
	{ 
		ledbat_head = ledbat->next;
	}
	else 
	{
		for (p = ledbat_head; 
			 p != NULL && p->next != NULL && p->next != ledbat; 
			 p = p->next);
		p->next = ledbat->next;
	}
	ledbat_open_sockets--;
}

/**
 * tcp_ledbat_init
 *
 * Init all required variables.
 * Clone the handling from Vegas module implementation.
 */
static void tcp_ledbat_init(struct sock *sk)
{
	struct ledbat *ledbat = inet_csk_ca(sk);

	ledbat_init_circbuf( &(ledbat->base_history), 
				  base_histo_len);

	ledbat_init_circbuf( &(ledbat->noise_filter), 
				  noise_filter_len);

	ledbat->last_rollover = 0;

	ledbat->flag = 0;
	ledbat->remote_hz = 0;
	ledbat->remote_ref_time = 0;
	ledbat->local_ref_time = 0;
	ledbat->sowd = 0;
	ledbat->next = ledbat_head;
	ledbat_head = ledbat;
	ledbat_open_sockets++;

	ledbat->target = target;

	if (do_ss) {
		ledbat->flag |= LEDBAT_CAN_SS;
	}
        
}

static u32 ledbat_min_circ_buff(struct owd_circ_buf *b) {
	if (b->first == b->next)
		return 0xffffffff;
	return b->buffer[b->min];
}

static
u32 ledbat_current_delay(struct ledbat *ledbat) {
	return ledbat_min_circ_buff(&(ledbat->noise_filter));
}

static
u32 ledbat_base_delay(struct ledbat *ledbat) {
	
	u32 res = 0xffffffff;
	struct ledbat *p;

	switch (min_impl) {
		case MIN_BASE_HISTO:
			//as in draft use the min of base_histo as min
			res = ledbat_min_circ_buff(&(ledbat->base_history));
			break;
		case MIN_GLOBAL:
			//use the min of all ledbat as min
			for (p = ledbat_head; p != NULL; p = p->next)
				res = min(res, ledbat_min_circ_buff(&(p->base_history)));
			break;
		case MIN_MAV:
			//use the moving average as min
			res = ledbat->sowd >> 3;
			break;
	}

	return res;
}

static
void print_delay(struct owd_circ_buf *cb) {
	u16 curr;
	
	curr = cb->first;

	printk(KERN_INFO "time %lu ", tcp_time_stamp);
	while (curr != cb->next) {
		printk(KERN_INFO "%u ", cb->buffer[curr]);
		curr = (curr + 1) % cb->len;
	}
	
	printk (KERN_INFO "min %u, len %u, first %u, next %u\n", 
		cb->buffer[cb->min],
		cb->len,
		cb->first,
		cb->next);
}

/**
 * tcp_ledbat_cong_avoid
 *
 */
static void tcp_ledbat_cong_avoid(struct sock *sk, 
				  u32 ack, u32 rtt, u32 in_flight, int flag)
{

	struct ledbat *ledbat = inet_csk_ca(sk);
	s32 queue_delay;
	s32 offset;
	s32 cwnd;
	u32 max_cwnd; 

	/*if no valid data return*/
	if (!(ledbat->flag & LEDBAT_VALID_OWD))
		return;

	struct tcp_sock *tp = tcp_sk(sk);

	max_cwnd = ((u32) (tp->snd_cwnd))*target;

	if (!tcp_is_cwnd_limited(sk, in_flight))
		return;

	if (tp->snd_cwnd <= 1) {
		ledbat->flag |= LEDBAT_CAN_SS;
	}

    u32	ssthresh = (do_ss == 2 ? ledbat_ssthresh : tp->snd_ssthresh);

	if (do_ss && tp->snd_cwnd <= tp->snd_ssthresh && (ledbat->flag & LEDBAT_CAN_SS)) {
		printf("slow_start!!! clamp %u cwnd %lu sshthresh %lu \n", 
			tp->snd_cwnd_clamp, tp->snd_cwnd, tp->snd_ssthresh);
		tcp_slow_start(tp);
		return;
	} else {
		ledbat->flag &= ~LEDBAT_CAN_SS;
	}

	queue_delay = ledbat_current_delay(ledbat) - ledbat_base_delay(ledbat);
	offset = ledbat->target - ((s32) queue_delay);

	offset *= alpha1; offset /= alpha2;

	//do not ramp more than TCP
	if (offset > ledbat->target)
		offset = ledbat->target;
        /* if (offset < -ledbat->target)     */
        /*         offset = -ledbat->target; */
	
	printk (
	KERN_INFO "time %lu, queue_delay %ld, offset %ld cwnd_cnt %ld, cwnd %lu, min %lu, target %u ", 
		tcp_time_stamp, queue_delay, offset, tp->snd_cwnd_cnt, tp->snd_cwnd, 
		ledbat_base_delay(ledbat),
		ledbat->target);

	//calculate the new cwnd_cnt
	cwnd = tp->snd_cwnd_cnt + offset;
	if (cwnd >= 0) 
	{
		//if we have a positive number
		//update the cwnd_count
		tp->snd_cwnd_cnt = cwnd;
		if (tp->snd_cwnd_cnt >= max_cwnd) 
		{
			//if we are here we need to increase the cwnd
			if (tp->snd_cwnd < tp->snd_cwnd_clamp)
				tp->snd_cwnd++;
			tp->snd_cwnd_cnt = 0;
			
		} 
	} 
	else 
	{
		//we need to decrease the cwnd
		//but we do not want to set it to 0!!!
		if (tp->snd_cwnd > 1) 
		{
			tp->snd_cwnd--;
			//set the cwnd_cnt to the max value - target
			tp->snd_cwnd_cnt = (tp->snd_cwnd - 1) * target;
		 } 
		 else 
		 {
			tp->snd_cwnd_cnt = 0;
		 }
	}

}

/**
 * tcp_ledbat_remote_hz_estimator
 *
 * Estimate remote HZ.
 * We keep on updating the estimated value, where original TCP-LP
 * implementation only guest it for once and use forever.
 */
static u32 tcp_ledbat_remote_hz_estimator(struct sock *sk)
{
	struct tcp_sock *tp = tcp_sk(sk);
	struct ledbat *ledbat = inet_csk_ca(sk);
	s64 rhz = ledbat->remote_hz << 6;	/* remote HZ << 6 */
	s64 m = 0;

	if (ledbat->last_rollover == 0) 
		ledbat->last_rollover = tcp_time_stamp;

	/* not yet record reference time
	 * go away!! record it before come back!! */
	if (ledbat->remote_ref_time == 0 || ledbat->local_ref_time == 0)
		goto out;

	/* we can't calc remote HZ with no different!! */
	if (tp->rx_opt.rcv_tsval == ledbat->remote_ref_time
	    || tp->rx_opt.rcv_tsecr == ledbat->local_ref_time)
		goto out;

	m = HZ * (tp->rx_opt.rcv_tsval -
		  ledbat->remote_ref_time) / (tp->rx_opt.rcv_tsecr -
					  ledbat->local_ref_time);
	if (m < 0)
		m = -m;

	if (rhz > 0) {
		m -= rhz >> 6;	/* m is now error in remote HZ est */
		rhz += m;	/* 63/64 old + 1/64 new */
	} else
		rhz = m << 6;

 out:
	/* record time for successful remote HZ calc */
	if ((rhz >> 6) > 0)
		ledbat->flag |= LEDBAT_VALID_RHZ;
	else
		ledbat->flag &= ~LEDBAT_VALID_RHZ;

	/* record reference time stamp */
	ledbat->remote_ref_time = tp->rx_opt.rcv_tsval;
	ledbat->local_ref_time = tp->rx_opt.rcv_tsecr;

	return rhz >> 6;
}

/**
 * tcp_ledbat_owd_calculator
 *
 * XXX old comment from lp authors...
 *
 * Calculate one way delay (in relative format).
 * Original implement OWD as minus of remote time difference to local time
 * difference directly. As this time difference just simply equal to RTT, when
 * the network status is stable, remote RTT will equal to local RTT, and result
 * OWD into zero.
 * It seems to be a bug and so we fixed it.
 */
static u32 tcp_ledbat_owd_calculator(struct sock *sk)
{
	struct tcp_sock *tp = tcp_sk(sk);
	struct ledbat *ledbat = inet_csk_ca(sk);
	s64 owd = 0;

	ledbat->remote_hz = tcp_ledbat_remote_hz_estimator(sk);

	if (ledbat->flag & LEDBAT_VALID_RHZ) {
		owd =
		    tp->rx_opt.rcv_tsval * (LP_RESOL / ledbat->remote_hz) -
		    tp->rx_opt.rcv_tsecr * (LP_RESOL / HZ);
		if (owd < 0)
			owd = -owd;
	}
	
	owd = tp->rx_opt.rcv_tsval - tp->rx_opt.rcv_tsecr;
	//owd *= LP_RESOL;

	if (owd < 0) {
		printf("owd < 0 !!!!\n");
		owd = -owd;
	}

	if (owd > 0)
		ledbat->flag |= LEDBAT_VALID_OWD;
	else
		ledbat->flag &= ~ LEDBAT_VALID_OWD;

	return owd;
}

static void ledbat_add_delay(struct owd_circ_buf *cb, u32 owd) {

	u8 i;

	if (cb->next == cb->first) 
	{ 
		/*buffer is empty */
		cb->buffer[cb->next] = owd;
		cb->min = cb->next;
		cb->next++;
		return;
	}

	/*set the new delay*/
	cb->buffer[cb->next] = owd;
	/*update the min if it is the case*/
	if (owd < cb->buffer[cb->min]) 
	{
		cb->min = cb->next;
	}
	/*increment the next pointer*/
	cb->next = (cb->next + 1) % cb->len;

	if (cb->next == cb->first)
	{ /* we need to discard the first element*/ 

		if ( cb->min == cb->first ) {
			/* we discard the min, search a new one*/
			cb->min = i = (cb->first + 1) % cb->len;
			while ( i != cb->next ) {
				if (cb->buffer[i] < cb->buffer[cb->min])
					cb->min = i;
				i = (i+1) % cb->len;
			}
		}

		/*move the first*/
		cb->first = (cb->first + 1) % cb->len;
	}
}

static void ledbat_update_current_delay(struct ledbat *ledbat, u32 owd)
{
	ledbat_add_delay(&(ledbat->noise_filter), owd); 
	printk (KERN_INFO "added delay to noisefilter %lu\n", owd);
	print_delay(&(ledbat->noise_filter));
}

static void ledbat_update_base_delay(struct ledbat *ledbat, u32 owd)
{
	u32 last;
	struct owd_circ_buf *cb = &(ledbat->base_history);

	if (ledbat->base_history.next == ledbat->base_history.first) 
	{
		/*empty circular buffer*/
		ledbat_add_delay(cb, owd); 
		return;
	}

	if (tcp_time_stamp - ledbat->last_rollover > 60 * HZ) {
		/*we have finished a minute*/
		printk (KERN_INFO "time %lu, new rollover \n",
			tcp_time_stamp);
		ledbat->last_rollover = tcp_time_stamp;
		ledbat_add_delay(cb, owd); 
	}
	else
	{
		/*simply update the last value and the min if it is the case*/
		last = (cb->next + cb->len - 1) % cb->len;

		if ( owd < cb->buffer[last]) {
			cb->buffer[last] = owd;
			if (owd < cb->buffer[cb->min])
				cb->min = last;
		}
		
	}
	printk (KERN_INFO "added delay to base_history %s", "\n");
	print_delay(&(ledbat->base_history));
}

/**
 * tcp_ledbat_rtt_sample
 *
 * - calculate the owd
 * - add the delay to noise filter
 * - if new minute add the delay to base delay or update last delay 
 */
static void tcp_ledbat_rtt_sample(struct sock *sk, u32 rtt)
{
	struct ledbat *ledbat = inet_csk_ca(sk);
	s64 mowd = tcp_ledbat_owd_calculator(sk);

	/*sorry that we don't have valid data */
	if (!(ledbat->flag & LEDBAT_VALID_RHZ) || !(ledbat->flag & LEDBAT_VALID_OWD))
		return;

	ledbat_update_current_delay(ledbat, mowd);
	ledbat_update_base_delay(ledbat, mowd);
	
	/* calc for smoothed owd */
	if (ledbat->sowd != 0) {
		mowd -= ledbat->sowd >> 3;	/* m is now error in owd est */
		ledbat->sowd += mowd;	/* owd = 7/8 owd + 1/8 new */
	} else
		ledbat->sowd = mowd << 3;	/* take the measured time be owd */
}

/**
 * tcp_ledbat_pkts_acked
 *
 * Implementation of pkts_acked.
 */
static void tcp_ledbat_pkts_acked(struct sock *sk, u32 num_acked, ktime_t last)
{
	//struct tcp_sock *tp = tcp_sk(sk);
	//struct ledbat *ledbat = inet_csk_ca(sk);

	if (!ktime_equal(last, net_invalid_timestamp()))
		tcp_ledbat_rtt_sample(sk,  ktime_to_us(net_timedelta(last)));

}

static u32 tcp_ledbat_min_cwnd(const struct sock *sk)
{
	struct tcp_sock *tp = tcp_sk(sk);
	unsigned int res = tcp_reno_min_cwnd(sk);
	unsigned int prev = tp->snd_cwnd;
	printf("%lu perdo!\n", tcp_time_stamp);
	printk(KERN_INFO "time %lu, detected loss, cwnd set from %u to %u\n", tcp_time_stamp, prev, res);
	return res;
}

static
u32 tcp_ledbat_ssthresh(struct sock *sk)
{
	u32 res;
	
	switch (do_ss) {
		case 0:
		case 1:
		default:
			res = tcp_reno_ssthresh(sk);
			break;
		case 2:
			res = ledbat_ssthresh;
			break;
	}

	return res;
}

static
void tcp_ledbat_set_state(struct sock *sk, u8 new_state) 
{
	assert(new_state != TCP_CA_Recovery);
	return;
}

static struct tcp_congestion_ops tcp_ledbat = {
	.flags = TCP_CONG_RTT_STAMP,
	.init = tcp_ledbat_init,
	.release = tcp_ledbat_release,
	.ssthresh = tcp_ledbat_ssthresh,
	.cong_avoid = tcp_ledbat_cong_avoid,
	.min_cwnd = tcp_ledbat_min_cwnd,
	.pkts_acked = tcp_ledbat_pkts_acked,
	.set_state = tcp_ledbat_set_state,

	.owner = THIS_MODULE,
	.name = "ledbat"
};

static int __init tcp_ledbat_register(void)
{
	BUILD_BUG_ON(sizeof(struct ledbat) > ICSK_CA_PRIV_SIZE);
	return tcp_register_congestion_control(&tcp_ledbat);
}

static void __exit tcp_ledbat_unregister(void)
{
	tcp_unregister_congestion_control(&tcp_ledbat);
}

module_init(tcp_ledbat_register);
module_exit(tcp_ledbat_unregister);

MODULE_AUTHOR("Silvio Valenti");
MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("TCP Ledbat");
#undef NS_PROTOCOL
