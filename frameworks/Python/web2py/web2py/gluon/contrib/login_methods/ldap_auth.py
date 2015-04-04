# -*- coding: utf-8 -*-
#
# last tinkered with by korylprince at gmail.com on 2012-07-12
#

import sys
import logging
try:
    import ldap
    import ldap.filter
    ldap.set_option(ldap.OPT_REFERRALS, 0)
except Exception, e:
    logging.error('missing ldap, try "easy_install python-ldap"')
    raise e


def ldap_auth(server='ldap', port=None,
              base_dn='ou=users,dc=domain,dc=com',
              mode='uid', secure=False, 
              cert_path=None, cert_file=None,              
              cacert_path=None, cacert_file=None, key_file=None,
              bind_dn=None, bind_pw=None, filterstr='objectClass=*',
              username_attrib='uid',
              custom_scope='subtree',
              allowed_groups=None,
              manage_user=False,
              user_firstname_attrib='cn:1',
              user_lastname_attrib='cn:2',
              user_mail_attrib='mail',
              manage_groups=False,
              db=None,
              group_dn=None,
              group_name_attrib='cn',
              group_member_attrib='memberUid',
              group_filterstr='objectClass=*',
              logging_level='error'):

    """
    to use ldap login with MS Active Directory:

        from gluon.contrib.login_methods.ldap_auth import ldap_auth
        auth.settings.login_methods.append(ldap_auth(
            mode='ad', server='my.domain.controller',
            base_dn='ou=Users,dc=domain,dc=com'))

    to use ldap login with Notes Domino:

        auth.settings.login_methods.append(ldap_auth(
            mode='domino',server='my.domino.server'))

    to use ldap login with OpenLDAP:

        auth.settings.login_methods.append(ldap_auth(
            server='my.ldap.server', base_dn='ou=Users,dc=domain,dc=com'))

    to use ldap login with OpenLDAP and subtree search and (optionally)
    multiple DNs:

        auth.settings.login_methods.append(ldap_auth(
            mode='uid_r', server='my.ldap.server',
            base_dn=['ou=Users,dc=domain,dc=com','ou=Staff,dc=domain,dc=com']))

    or (if using CN):

        auth.settings.login_methods.append(ldap_auth(
            mode='cn', server='my.ldap.server',
            base_dn='ou=Users,dc=domain,dc=com'))

    or you can full customize the search for user:

        auth.settings.login_methods.append(ldap_auth(
            mode='custom', server='my.ldap.server',
            base_dn='ou=Users,dc=domain,dc=com',
            username_attrib='uid',
            custom_scope='subtree'))

    the custom_scope can be: base, onelevel, subtree.

    If using secure ldaps:// pass secure=True and cert_path="..."
    If ldap is using GnuTLS then you need cert_file="..." instead cert_path
    because cert_path isn't implemented in GnuTLS :(

    If you need to bind to the directory with an admin account in order to
    search it then specify bind_dn & bind_pw to use for this.
    - currently only implemented for Active Directory

    If you need to restrict the set of allowed users (e.g. to members of a
    department) then specify an rfc4515 search filter string.
    - currently only implemented for mode in ['ad', 'company', 'uid_r']

    You can manage user attributes first name, last name, email from ldap:
        auth.settings.login_methods.append(ldap_auth(...as usual...,
            manage_user=True,
            user_firstname_attrib='cn:1',
            user_lastname_attrib='cn:2',
            user_mail_attrib='mail'
           ))

    Where:
    manage_user - let web2py handle user data from ldap
    user_firstname_attrib - the attribute containing the user's first name
                            optionally you can specify parts.
                            Example: cn: "John Smith" - 'cn:1'='John'
    user_lastname_attrib - the attribute containing the user's last name
                            optionally you can specify parts.
                            Example: cn: "John Smith" - 'cn:2'='Smith'
    user_mail_attrib - the attribute containing the user's email address


    If you need group control from ldap to web2py app's database feel free
    to set:

        auth.settings.login_methods.append(ldap_auth(...as usual...,
            manage_groups=True,
            db=db,
            group_dn='ou=Groups,dc=domain,dc=com',
            group_name_attrib='cn',
            group_member_attrib='memberUid',
            group_filterstr='objectClass=*'
           ))

        Where:
        manage_group - let web2py handle the groups from ldap
        db - is the database object (need to have auth_user, auth_group,
            auth_membership)
        group_dn - the ldap branch of the groups
        group_name_attrib - the attribute where the group name is stored
        group_member_attrib - the attribute containing the group members name
        group_filterstr - as the filterstr but for group select

    You can restrict login access to specific groups if you specify:

        auth.settings.login_methods.append(ldap_auth(...as usual...,
            allowed_groups=[...],
            group_dn='ou=Groups,dc=domain,dc=com',
            group_name_attrib='cn',
            group_member_attrib='memberUid',#use 'member' for Active Directory
            group_filterstr='objectClass=*'
           ))

        Where:
        allowed_groups - a list with allowed ldap group names
        group_dn - the ldap branch of the groups
        group_name_attrib - the attribute where the group name is stored
        group_member_attrib - the attribute containing the group members name
        group_filterstr - as the filterstr but for group select

    If using Active Directory you must specify bind_dn and bind_pw for
    allowed_groups unless anonymous bind works.

    You can set the logging level with the "logging_level" parameter, default
    is "error" and can be set to error, warning, info, debug.
    """
    logger = logging.getLogger('web2py.auth.ldap_auth')
    if logging_level == 'error':
        logger.setLevel(logging.ERROR)
    elif logging_level == 'warning':
        logger.setLevel(logging.WARNING)
    elif logging_level == 'info':
        logger.setLevel(logging.INFO)
    elif logging_level == 'debug':
        logger.setLevel(logging.DEBUG)

    def ldap_auth_aux(username,
                      password,
                      ldap_server=server,
                      ldap_port=port,
                      ldap_basedn=base_dn,
                      ldap_mode=mode,
                      ldap_binddn=bind_dn,
                      ldap_bindpw=bind_pw,
                      secure=secure,
                      cert_path=cert_path,
                      cert_file=cert_file,
                      cacert_file=cacert_file,
                      key_file=key_file,
                      filterstr=filterstr,
                      username_attrib=username_attrib,
                      custom_scope=custom_scope,
                      manage_user=manage_user,
                      user_firstname_attrib=user_firstname_attrib,
                      user_lastname_attrib=user_lastname_attrib,
                      user_mail_attrib=user_mail_attrib,
                      manage_groups=manage_groups,
                      allowed_groups=allowed_groups,
                      db=db):
        if password == '':  # http://tools.ietf.org/html/rfc4513#section-5.1.2
            logger.warning('blank password not allowed')
            return False
        logger.debug('mode: [%s] manage_user: [%s] custom_scope: [%s]'
                     ' manage_groups: [%s]' % (str(mode), str(manage_user),
                     str(custom_scope), str(manage_groups)))
        if manage_user:
            if user_firstname_attrib.count(':') > 0:
                (user_firstname_attrib,
                 user_firstname_part) = user_firstname_attrib.split(':', 1)
                user_firstname_part = (int(user_firstname_part) - 1)
            else:
                user_firstname_part = None
            if user_lastname_attrib.count(':') > 0:
                (user_lastname_attrib,
                 user_lastname_part) = user_lastname_attrib.split(':', 1)
                user_lastname_part = (int(user_lastname_part) - 1)
            else:
                user_lastname_part = None
            user_firstname_attrib = ldap.filter.escape_filter_chars(
                user_firstname_attrib)
            user_lastname_attrib = ldap.filter.escape_filter_chars(
                user_lastname_attrib)
            user_mail_attrib = ldap.filter.escape_filter_chars(
                user_mail_attrib)
        try:
            if allowed_groups:
                if not is_user_in_allowed_groups(username, password):
                    return False
            con = init_ldap()
            if ldap_mode == 'ad':
                # Microsoft Active Directory
                if '@' not in username:
                    domain = []
                    for x in ldap_basedn.split(','):
                        if "DC=" in x.upper():
                            domain.append(x.split('=')[-1])
                    username = "%s@%s" % (username, '.'.join(domain))
                username_bare = username.split("@")[0]
                con.set_option(ldap.OPT_PROTOCOL_VERSION, 3)
                # In cases where ForestDnsZones and DomainDnsZones are found,
                # result will look like the following:
                # ['ldap://ForestDnsZones.domain.com/DC=ForestDnsZones,
                #    DC=domain,DC=com']
                if ldap_binddn:
                    # need to search directory with an admin account 1st
                    con.simple_bind_s(ldap_binddn, ldap_bindpw)
                else:
                    # credentials should be in the form of username@domain.tld
                    con.simple_bind_s(username, password)
                # this will throw an index error if the account is not found
                # in the ldap_basedn
                requested_attrs = ['sAMAccountName']
                if manage_user:
                    requested_attrs.extend([user_firstname_attrib,
                                           user_lastname_attrib,
                                           user_mail_attrib])
                result = con.search_ext_s(
                    ldap_basedn, ldap.SCOPE_SUBTREE,
                    "(&(sAMAccountName=%s)(%s))" % (
                                ldap.filter.escape_filter_chars(username_bare),
                                filterstr),
                    requested_attrs)[0][1]
                if not isinstance(result, dict):
                    # result should be a dict in the form
                    # {'sAMAccountName': [username_bare]}
                    logger.warning('User [%s] not found!' % username)
                    return False
                if ldap_binddn:
                    # We know the user exists & is in the correct OU
                    # so now we just check the password
                    con.simple_bind_s(username, password)
                username = username_bare

            if ldap_mode == 'domino':
                # Notes Domino
                if "@" in username:
                    username = username.split("@")[0]
                con.simple_bind_s(username, password)
                if manage_user:
                    # TODO: sorry I have no clue how to query attrs in domino
                    result = {user_firstname_attrib: username,
                              user_lastname_attrib: None,
                              user_mail_attrib: None}

            if ldap_mode == 'cn':
                # OpenLDAP (CN)
                if ldap_binddn and ldap_bindpw:
                    con.simple_bind_s(ldap_binddn, ldap_bindpw)
                dn = "cn=" + username + "," + ldap_basedn
                con.simple_bind_s(dn, password)
                if manage_user:
                    result = con.search_s(dn, ldap.SCOPE_BASE,
                                          "(objectClass=*)",
                                          [user_firstname_attrib,
                                          user_lastname_attrib,
                                          user_mail_attrib])[0][1]

            if ldap_mode == 'uid':
                # OpenLDAP (UID)
                if ldap_binddn and ldap_bindpw:
                    con.simple_bind_s(ldap_binddn, ldap_bindpw)
                    dn = "uid=" + username + "," + ldap_basedn
                    dn = con.search_s(ldap_basedn, ldap.SCOPE_SUBTREE, "(uid=%s)"%username, [''])[0][0]
                else:
                    dn = "uid=" + username + "," + ldap_basedn
                con.simple_bind_s(dn, password)
                if manage_user:
                    result = con.search_s(dn, ldap.SCOPE_BASE,
                                          "(objectClass=*)",
                                          [user_firstname_attrib,
                                          user_lastname_attrib,
                                          user_mail_attrib])[0][1]

            if ldap_mode == 'company':
                # no DNs or password needed to search directory
                dn = ""
                pw = ""
                # bind anonymously
                con.simple_bind_s(dn, pw)
                # search by e-mail address
                filter = '(&(mail=%s)(%s))' % (
                                ldap.filter.escape_filter_chars(username),
                                filterstr)
                # find the uid
                attrs = ['uid']
                if manage_user:
                    attrs.extend([user_firstname_attrib,
                                  user_lastname_attrib,
                                  user_mail_attrib])
                # perform the actual search
                company_search_result = con.search_s(ldap_basedn,
                                                     ldap.SCOPE_SUBTREE,
                                                     filter, attrs)
                dn = company_search_result[0][0]
                result = company_search_result[0][1]
                # perform the real authentication test
                con.simple_bind_s(dn, password)

            if ldap_mode == 'uid_r':
                # OpenLDAP (UID) with subtree search and multiple DNs
                if isinstance(ldap_basedn, list):
                    basedns = ldap_basedn
                else:
                    basedns = [ldap_basedn]
                filter = '(&(uid=%s)(%s))' % (
                    ldap.filter.escape_filter_chars(username), filterstr)
                found = False
                for basedn in basedns:
                    try:
                        result = con.search_s(basedn, ldap.SCOPE_SUBTREE,
                                              filter)
                        if result:
                            user_dn = result[0][0]
                            # Check the password
                            con.simple_bind_s(user_dn, password)
                            found = True
                            break
                    except ldap.LDAPError, detail:
                        (exc_type, exc_value) = sys.exc_info()[:2]
                        logger.warning(
                        "ldap_auth: searching %s for %s resulted in %s: %s\n" %
                                       (basedn, filter, exc_type, exc_value)
                                       )
                if not found:
                    logger.warning('User [%s] not found!' % username)
                    return False
                result = result[0][1]
            if ldap_mode == 'custom':
                # OpenLDAP (username_attrs) with subtree search and
                # multiple DNs
                if isinstance(ldap_basedn, list):
                    basedns = ldap_basedn
                else:
                    basedns = [ldap_basedn]
                filter = '(&(%s=%s)(%s))' % (username_attrib,
                                             ldap.filter.escape_filter_chars(
                                                 username),
                                             filterstr)
                if custom_scope == 'subtree':
                    ldap_scope = ldap.SCOPE_SUBTREE
                elif custom_scope == 'base':
                    ldap_scope = ldap.SCOPE_BASE
                elif custom_scope == 'onelevel':
                    ldap_scope = ldap.SCOPE_ONELEVEL
                found = False
                for basedn in basedns:
                    try:
                        result = con.search_s(basedn, ldap_scope, filter)
                        if result:
                            user_dn = result[0][0]
                            # Check the password
                            con.simple_bind_s(user_dn, password)
                            found = True
                            break
                    except ldap.LDAPError, detail:
                        (exc_type, exc_value) = sys.exc_info()[:2]
                        logger.warning(
                        "ldap_auth: searching %s for %s resulted in %s: %s\n" %
                                       (basedn, filter, exc_type, exc_value)
                                       )
                if not found:
                    logger.warning('User [%s] not found!' % username)
                    return False
                result = result[0][1]
            if manage_user:
                logger.info('[%s] Manage user data' % str(username))
                try:
                    if user_firstname_part is not None:
                        store_user_firstname = result[user_firstname_attrib][
                            0].split(' ', 1)[user_firstname_part]
                    else:
                        store_user_firstname = result[user_firstname_attrib][0]
                except KeyError, e:
                    store_user_firstname = None
                try:
                    if user_lastname_part is not None:
                        store_user_lastname = result[user_lastname_attrib][
                            0].split(' ', 1)[user_lastname_part]
                    else:
                        store_user_lastname = result[user_lastname_attrib][0]
                except KeyError, e:
                    store_user_lastname = None
                try:
                    store_user_mail = result[user_mail_attrib][0]
                except KeyError, e:
                    store_user_mail = None
                try:
                    #
                    # user as username
                    # #################
                    user_in_db = db(db.auth_user.username == username)
                    if user_in_db.count() > 0:
                        user_in_db.update(first_name=store_user_firstname,
                                          last_name=store_user_lastname,
                                          email=store_user_mail)
                    else:
                        db.auth_user.insert(first_name=store_user_firstname,
                                            last_name=store_user_lastname,
                                            email=store_user_mail,
                                            username=username)
                except:
                    #
                    # user as email
                    # ##############
                    user_in_db = db(db.auth_user.email == username)
                    if user_in_db.count() > 0:
                        user_in_db.update(first_name=store_user_firstname,
                                          last_name=store_user_lastname)
                    else:
                        db.auth_user.insert(first_name=store_user_firstname,
                                            last_name=store_user_lastname,
                                            email=username)
            con.unbind()

            if manage_groups:
                if not do_manage_groups(username, password):
                    return False
            return True
        except ldap.INVALID_CREDENTIALS, e:
            return False
        except ldap.LDAPError, e:
            import traceback
            logger.warning('[%s] Error in ldap processing' % str(username))
            logger.debug(traceback.format_exc())
            return False
        except IndexError, ex:  # for AD membership test
            import traceback
            logger.warning('[%s] Ldap result indexing error' % str(username))
            logger.debug(traceback.format_exc())
            return False

    def is_user_in_allowed_groups(username,
                                  password=None,
                                  allowed_groups=allowed_groups):
        """
        Figure out if the username is a member of an allowed group
        in ldap or not
        """
        #
        # Get all group name where the user is in actually in ldap
        # #########################################################
        ldap_groups_of_the_user = get_user_groups_from_ldap(username, password)

        # search for allowed group names
        if type(allowed_groups) != type(list()):
            allowed_groups = [allowed_groups]
        for group in allowed_groups:
            if ldap_groups_of_the_user.count(group) > 0:
                # Match
                return True
        # No match
        return False

    def do_manage_groups(username,
                         password=None,
                         db=db):
        """
        Manage user groups

        Get all user's group from ldap and refresh the already stored
        ones in web2py's application database or create new groups
        according to ldap.
        """
        logger.info('[%s] Manage user groups' % str(username))
        try:
            #
            # Get all group name where the user is in actually in ldap
            # #########################################################
            ldap_groups_of_the_user = get_user_groups_from_ldap(
                username, password)

            #
            # Get all group name where the user is in actually in local db
            # #############################################################
            try:
                db_user_id = db(db.auth_user.username == username).select(
                    db.auth_user.id).first().id
            except:
                try:
                    db_user_id = db(db.auth_user.email == username).select(
                        db.auth_user.id).first().id
                except AttributeError, e:
                    #
                    # There is no user in local db
                    # We create one
                    # ##############################
                    try:
                        db_user_id = db.auth_user.insert(username=username,
                                                         first_name=username)
                    except AttributeError, e:
                        db_user_id = db.auth_user.insert(email=username,
                                                         first_name=username)
            if not db_user_id:
                logging.error(
                    'There is no username or email for %s!' % username)
                raise
            db_group_search = db((db.auth_membership.user_id == db_user_id) &
                            (db.auth_user.id == db.auth_membership.user_id) &
                            (db.auth_group.id == db.auth_membership.group_id))
            db_groups_of_the_user = list()
            db_group_id = dict()

            if db_group_search.count() > 0:
                for group in db_group_search.select(db.auth_group.id,
                                                    db.auth_group.role,
                                                    distinct=True):
                    db_group_id[group.role] = group.id
                    db_groups_of_the_user.append(group.role)
            logging.debug('db groups of user %s: %s' %
                          (username, str(db_groups_of_the_user)))

            #
            # Delete user membership from groups where user is not anymore
            # #############################################################
            for group_to_del in db_groups_of_the_user:
                if ldap_groups_of_the_user.count(group_to_del) == 0:
                    db((db.auth_membership.user_id == db_user_id) &
                       (db.auth_membership.group_id == \
                         db_group_id[group_to_del])).delete()

            #
            # Create user membership in groups where user is not in already
            # ##############################################################
            for group_to_add in ldap_groups_of_the_user:
                if db_groups_of_the_user.count(group_to_add) == 0:
                    if db(db.auth_group.role == group_to_add).count() == 0:
                        gid = db.auth_group.insert(role=group_to_add,
                                            description='Generated from LDAP')
                    else:
                        gid = db(db.auth_group.role == group_to_add).select(
                            db.auth_group.id).first().id
                    db.auth_membership.insert(user_id=db_user_id,
                                              group_id=gid)
        except:
            logger.warning("[%s] Groups are not managed successfully!" %
                           str(username))
            import traceback
            logger.debug(traceback.format_exc())
            return False
        return True

    def init_ldap(ldap_server=server,
                  ldap_port=port,
                  ldap_basedn=base_dn,
                  ldap_mode=mode,
                  secure=secure,
                  cert_path=cert_path,
                  cert_file=cert_file,
                  cacert_file=cacert_file,
                  key_file=key_file):
        """
        Inicialize ldap connection
        """
        logger.info('[%s] Initialize ldap connection' % str(ldap_server))
        if secure:
            if not ldap_port:
                ldap_port = 636
                
            if cacert_path:
                ldap.set_option(ldap.OPT_X_TLS_CACERTDIR, cacert_path)
                
            if cacert_file:
                ldap.set_option(ldap.OPT_X_TLS_REQUIRE_CERT, ldap.OPT_X_TLS_NEVER)
                ldap.set_option(ldap.OPT_X_TLS_CACERTFILE, cacert_file)
            if cert_file:
                ldap.set_option(ldap.OPT_X_TLS_CERTFILE, cert_file)
            if key_file:
                ldap.set_option(ldap.OPT_X_TLS_KEYFILE, key_file)
                
            con = ldap.initialize("ldaps://" + ldap_server + ":" + str(ldap_port))
        else:
            if not ldap_port:
                ldap_port = 389
            con = ldap.initialize(
                "ldap://" + ldap_server + ":" + str(ldap_port))
        return con

    def get_user_groups_from_ldap(username,
                                  password=None,
                                  base_dn=base_dn,
                                  ldap_binddn=bind_dn,
                                  ldap_bindpw=bind_pw,
                                  group_dn=group_dn,
                                  group_name_attrib=group_name_attrib,
                                  group_member_attrib=group_member_attrib,
                                  group_filterstr=group_filterstr,
                                  ldap_mode=mode):
        """
        Get all group names from ldap where the user is in
        """
        logger.info('[%s] Get user groups from ldap' % str(username))
        #
        # Get all group name where the user is in actually in ldap
        # #########################################################
        # Initialize ldap
        if not group_dn:
            group_dn = base_dn
        con = init_ldap()
        logger.debug('Username init: [%s]' % username)
        if ldap_mode == 'ad':
            #
            # Get the AD username
            # ####################
            if '@' not in username:
                domain = []
                for x in base_dn.split(','):
                    if "DC=" in x.upper():
                        domain.append(x.split('=')[-1])
                username = "%s@%s" % (username, '.'.join(domain))
            username_bare = username.split("@")[0]
            con.set_option(ldap.OPT_PROTOCOL_VERSION, 3)
            # In cases where ForestDnsZones and DomainDnsZones are found,
            # result will look like the following:
            # ['ldap://ForestDnsZones.domain.com/DC=ForestDnsZones,
            #     DC=domain,DC=com']
            if ldap_binddn:
                # need to search directory with an admin account 1st
                con.simple_bind_s(ldap_binddn, ldap_bindpw)
                logger.debug('Ldap bind connect...')
            else:
                # credentials should be in the form of username@domain.tld
                con.simple_bind_s(username, password)
                logger.debug('Ldap username connect...')
            # We have to use the full string
            username = con.search_ext_s(base_dn, ldap.SCOPE_SUBTREE,
                                        "(&(sAMAccountName=%s)(%s))" %
                            (ldap.filter.escape_filter_chars(username_bare),
                            filterstr), ["cn"])[0][0]
        else:
            if ldap_binddn:
                # need to search directory with an bind_dn account 1st
                con.simple_bind_s(ldap_binddn, ldap_bindpw)
            else:
                # bind as anonymous
                con.simple_bind_s('', '')
                
        # if username is None, return empty list
        if username is None:
            return list()
        # search for groups where user is in
        filter = '(&(%s=%s)(%s))' % (ldap.filter.escape_filter_chars(
                                                            group_member_attrib
                                                            ),
                                     ldap.filter.escape_filter_chars(username),
                                     group_filterstr)
        group_search_result = con.search_s(group_dn,
                                           ldap.SCOPE_SUBTREE,
                                           filter, [group_name_attrib])
        ldap_groups_of_the_user = list()
        for group_row in group_search_result:
            group = group_row[1]
            if type(group) == dict and group.has_key(group_name_attrib):
                ldap_groups_of_the_user.extend(group[group_name_attrib])

        con.unbind()
        logger.debug('User groups: %s' % ldap_groups_of_the_user)
        return list(ldap_groups_of_the_user)

    if filterstr[0] == '(' and filterstr[-1] == ')':  # rfc4515 syntax
        filterstr = filterstr[1:-1]  # parens added again where used
    return ldap_auth_aux
