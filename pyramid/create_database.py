from random import randint
from sqlalchemy.orm import sessionmaker
from sqlalchemy import create_engine
from frameworkbenchmarks.models import pg, DBSession, metadata, World, Fortune

if __name__ == "__main__":
    """
    Initialize database
    """
    World.__table__.drop(pg, checkfirst=True)
    Fortune.__table__.drop(pg, checkfirst=True)
    DBSession.commit()
    World.__table__.create(pg)
    Fortune.__table__.create(pg)
    DBSession.commit()
    DBSession.execute(
        World.__table__.insert([(num, randint(1, 10001)) for num in range(1, 10001)])
    )
    DBSession.execute(
        Fortune.__table__.insert([
            ("1", "fortune: No such file or directory"),
            ("2", "A computer scientist is someone who fixes things that aren't broken."),
            ("3", "After enough decimal places, nobody gives a damn."),
            ("4", "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1"),
            ("5", "A computer program does what you tell it to do, not what you want it to do."),
            ("6", "Emacs is a nice operating system, but I prefer UNIX. â€” Tom Christaensen"),
            ("7", "Any program that runs right is obsolete."),
            ("8", "A list is only as strong as its weakest link. â€” Donald Knuth"),
            ("9", "Feature: A bug with seniority."),
            ("10", "Computers make very fast, very accurate mistakes."),
            ("11", """<script>alert("This should not be displayed in a browser alert box.");</script>"""),
            ("12", "フレームワークのベンチマーク"),
        ])
    )
    DBSession.commit()


