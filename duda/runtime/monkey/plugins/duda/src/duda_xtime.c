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

#include "MKPlugin.h"
#include "duda_xtime.h"

/*
 * @OBJ_NAME: xtime
 * @OBJ_MENU: Time Handling
 * @OBJ_DESC: The xtime object provides a set of methods to retrieve and manipulate
 * the timing information based on unix timestamp
 */

struct duda_api_xtime *duda_xtime_object() {
    struct duda_api_xtime *t;

    t = mk_api->mem_alloc(sizeof(struct duda_api_xtime));
    t->now = duda_xtime_now;
    t->tomorrow = duda_xtime_tomorrow;
    t->next_hours = duda_xtime_next_hours;

    return t;
}

/*
 * @METHOD_NAME: now
 * @METHOD_DESC: Returns the current time in unix timestamp format
 * @METHOD_RETURN: Upon successful completion it returns the unix timestamp
 */
time_t duda_xtime_now()
{
    return mk_api->time_unix();
}

/*
 * @METHOD_NAME: tomorrow
 * @METHOD_DESC: Returns the unix timestamp for the next 24 hours from now
 * @METHOD_RETURN: Upon successful completion it returns the unix timestamp
 */
time_t duda_xtime_tomorrow()
{
    return (mk_api->time_unix() + TIME_DAY);
}

/*
 * @METHOD_NAME: next_hours
 * @METHOD_DESC: Returns the unix timestamp for the given next hours
 * @METHOD_PARAM: h Represent the number of hours to perform the offset
 * @METHOD_RETURN: Upon successful completion it returns the unix timestamp
 */
time_t duda_xtime_next_hours(int h)
{
    return (mk_api->time_unix() + (h * TIME_HOUR));
}
