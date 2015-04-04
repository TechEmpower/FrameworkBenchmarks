from gluon.contrib.pam import authenticate


def pam_auth():
    """
    to use pam_login:
    from gluon.contrib.login_methods.pam_auth import pam_auth
    auth.settings.login_methods.append(pam_auth())

    or

    auth.settings.actions_disabled=[
       'register','change_password','request_reset_password']
    auth.settings.login_methods=[pam_auth()]

    The latter method will not store the user password in auth_user.
    """

    def pam_auth_aux(username, password):
        return authenticate(username, password)

    return pam_auth_aux
