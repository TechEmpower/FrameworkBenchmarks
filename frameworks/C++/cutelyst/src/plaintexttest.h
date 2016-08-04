#ifndef PLAINTEXTTEST_H
#define PLAINTEXTTEST_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

class PlaintextTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit PlaintextTest(QObject *parent = 0);

    C_ATTR(plaintext, :Local :AutoArgs)
    void plaintext(Context *c);
};

#endif // PLAINTEXTTEST_H
