/*
 * copied from http://code.swtch.com/plan9port/src/0e6ae8ed3276/src/lib9/sendfd.c
 * modified
 * */

#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>

#ifndef CMSG_ALIGN
#       ifdef __sun__
#               define CMSG_ALIGN _CMSG_DATA_ALIGN
#       else
#               define CMSG_ALIGN(len) (((len)+sizeof(long)-1) & ~(sizeof(long)-1))
#       endif
#endif

#ifndef CMSG_SPACE
#       define CMSG_SPACE(len) (CMSG_ALIGN(sizeof(struct cmsghdr))+CMSG_ALIGN(len))
#endif

#ifndef CMSG_LEN
#       define CMSG_LEN(len) (CMSG_ALIGN(sizeof(struct cmsghdr))+(len))
#endif

int sendfd(int s, int fd, int flags) {
	char buf[1];
	struct iovec iov;
	struct msghdr msg;
	struct cmsghdr *cmsg;
	int n;
	char cms[CMSG_SPACE(sizeof(int))];

	buf[0] = 69;
	iov.iov_base = buf;
	iov.iov_len = 1;

	memset(&msg, 0, sizeof msg);
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;
	msg.msg_control = (caddr_t) cms;
	msg.msg_controllen = CMSG_LEN(sizeof(int));

	cmsg = CMSG_FIRSTHDR(&msg);
	cmsg->cmsg_len = CMSG_LEN(sizeof(int));
	cmsg->cmsg_level = SOL_SOCKET;
	cmsg->cmsg_type = SCM_RIGHTS;
	memmove(CMSG_DATA(cmsg), &fd, sizeof(int));

	if ((n = sendmsg(s, &msg, flags)) != (int) iov.iov_len) return -1;
	return 0;
}

int recvfd(int s, int flags) {
	int n;
	int fd;
	char buf[1];
	struct iovec iov;
	struct msghdr msg;
	struct cmsghdr *cmsg;
	char cms[CMSG_SPACE(sizeof(int))];

	iov.iov_base = buf;
	iov.iov_len = 1;

	memset(&msg, 0, sizeof msg);
	msg.msg_name = 0;
	msg.msg_namelen = 0;
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;

	msg.msg_control = (caddr_t) cms;
	msg.msg_controllen = sizeof cms;

	if ((n = recvmsg(s, &msg, flags)) <= 0) {
		//fprintf(stderr,"recvfd(fd=%i): recvmsg() returned %i; errno: %s\n",s,n,strerror(errno));
		return -1;
	}
	cmsg = CMSG_FIRSTHDR(&msg);
	if (cmsg == NULL) {
		errno = 0;
		fprintf(stderr,"recvfd(fd=%i): CMSG_FIRSTHDR(&msg) is NULL; n=%i, val=%i\n",s,n,(int)buf[0]);
		return -1;
	}
	memmove(&fd, CMSG_DATA(cmsg), sizeof(int));
	return fd;
}
