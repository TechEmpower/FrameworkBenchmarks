from gluon.fileutils import read_file, write_file

if DEMO_MODE or MULTI_USER_MODE:
    session.flash = T('disabled in demo mode')
    redirect(URL('default', 'site'))
if not have_mercurial:
    session.flash = T("Sorry, could not find mercurial installed")
    redirect(URL('default', 'design', args=request.args(0)))

_hgignore_content = """\
syntax: glob
*~
*.pyc
*.pyo
*.bak
*.bak2
cache/*
private/*
uploads/*
databases/*
sessions/*
errors/*
"""


def hg_repo(path):
    import os
    uio = ui.ui()
    uio.quiet = True
    if not os.environ.get('HGUSER') and not uio.config("ui", "username"):
        os.environ['HGUSER'] = 'web2py@localhost'
    try:
        repo = hg.repository(ui=uio, path=path)
    except:
        repo = hg.repository(ui=uio, path=path, create=True)
    hgignore = os.path.join(path, '.hgignore')
    if not os.path.exists(hgignore):
        write_file(hgignore, _hgignore_content)
    return repo


def commit():
    app = request.args(0)
    path = apath(app, r=request)
    repo = hg_repo(path)
    form = FORM(T('Comment:'), INPUT(_name='comment', requires=IS_NOT_EMPTY()),
                INPUT(_type='submit', _value=T('Commit')))
    if form.accepts(request.vars, session):
        oldid = repo[repo.lookup('.')]
        addremove(repo)
        repo.commit(text=form.vars.comment)
        if repo[repo.lookup('.')] == oldid:
            response.flash = T('no changes')
    try:
        files = TABLE(*[TR(file) for file in repo[repo.lookup('.')].files()])
        changes = TABLE(TR(TH('revision'), TH('description')))
        for change in repo.changelog:
            ctx = repo.changectx(change)
            revision, description = ctx.rev(), ctx.description()
            changes.append(TR(A(revision, _href=URL('revision',
                                                    args=(app, revision))),
                              description))
    except:
        files = []
        changes = []
    return dict(form=form, files=files, changes=changes, repo=repo)


def revision():
    app = request.args(0)
    path = apath(app, r=request)
    repo = hg_repo(path)
    revision = request.args(1)
    ctx = repo.changectx(revision)
    form = FORM(INPUT(_type='submit', _value=T('Revert')))
    if form.accepts(request.vars):
        hg.update(repo, revision)
        session.flash = T("reverted to revision %s") % ctx.rev()
        redirect(URL('default', 'design', args=app))
    return dict(
        files=ctx.files(),
        rev=str(ctx.rev()),
        desc=ctx.description(),
        form=form
    )
