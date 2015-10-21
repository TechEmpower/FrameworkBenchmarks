/*
	Copyright 2010, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * TestAllInOne.h
 *
 *  Created on: 18-Jun-2013
 *      Author: sumeetc
 */

#ifndef TESTALLINONE_H_
#define TESTALLINONE_H_
#include "TestObject1.h"
#include "TestObject.h"

namespace test
{
	namespace all
	{
		namespace in
		{
			namespace one
			{
				class TestAllInOne
				{
					public:
						TestObject to;
						com::obj::TestObject coto;
						vector<TestObject> vto;
						vector<com::obj::TestObject> vcoto;
						vector<vector<TestObject> > vvto;
						vector<vector<com::obj::TestObject> > vvcoto;

						string toString()
						{
							string out;
							out.append(to.toString());
							out.append("\n");
							out.append(coto.toString());
							out.append("\n");
							for (int var = 0; var < (int)vto.size(); ++var) {
								out.append(vto.at(var).toString());
								out.append("\n");
							}
							for (int var = 0; var < (int)vcoto.size(); ++var) {
								out.append(vcoto.at(var).toString());
								out.append("\n");
							}
							for (int var = 0; var < (int)vvto.size(); ++var) {
								for (int var1 = 0; var1 < (int)vvto.at(var).size(); ++var1) {
									out.append(vvto.at(var).at(var1).toString());
									out.append("\n");
								}
							}
							for (int var = 0; var < (int)vvcoto.size(); ++var) {
								for (int var1 = 0; var1 < (int)vvcoto.at(var).size(); ++var1) {
									out.append(vvcoto.at(var).at(var1).toString());
									out.append("\n");
								}
							}
							return out;
						}
				};
			}
		}
	}
}


#endif /* TESTALLINONE_H_ */
