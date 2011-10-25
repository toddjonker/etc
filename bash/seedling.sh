alias console="runseed webConsoleJetty"

pullseed()
{
    cp ~/dev/seedling/dist/seedling-deploy.zip $PROJECT_HOME/tools/
}

reseed()
{
    (cd ~/dev/seedling; . setup; ant dist.q) && pullseed
}

pullabbot()
{
    cp ~/dev/abbot/lib/abbot.jar $PROJECT_HOME/lib
}

reabbot()
{
    (cd ~/dev/abbot; /Users/todd/dev/seedling/tools/bin/ant jar)
    cp ~/dev/abbot/lib/abbot.jar $PROJECT_HOME/tools/abbot-*/lib
}


jdbc()
{
    java -cp build/classes/main/ezdata:lib/classes12.zip:lib/firebirdsql.jar:build/modules/runtime/lib/concurrent.jar:lib/mini-j2ee.jar:lib/log4j-core.jar:build/modules/runtime/lib/commons-lang.jar JdbcTool $*
}

peekdb()
{
    isql -user overlay -pass ezr ${PROJECT_HOME}/build/peak.fdb
}
