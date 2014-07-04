/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#ifndef DUDA_TIME_H
#define DUDA_TIME_H

#define TIME_HOUR   3600
#define TIME_DAY    (TIME_HOUR * 24)

struct duda_api_xtime {
    time_t (*now) ();
    time_t (*tomorrow) ();
    time_t (*next_hours) (int);
};

time_t duda_xtime_now();
time_t duda_xtime_tomorrow();
time_t duda_xtime_next_hours(int h);
struct duda_api_xtime *duda_xtime_object();

#endif
