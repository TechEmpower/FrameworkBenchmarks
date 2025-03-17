#!/usr/bin/env perl

use Path::Tiny qw(path);
use lib path(__FILE__)->parent->child('lib');
use KelpBench;

KelpBench->new->run;

