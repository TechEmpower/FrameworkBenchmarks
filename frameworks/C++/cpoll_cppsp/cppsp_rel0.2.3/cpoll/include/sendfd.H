/*
 * sendfd.H
 *
 *  Created on: Feb 4, 2013
 *      Author: xaxaxa
 */

#ifndef SENDFD_H_
#define SENDFD_H_

int sendfd(int s, int fd, int flags=0);
int recvfd(int s, int flags=0);

#endif /* SENDFD_H_ */
