def generate_concurrency_script(benchmarker_config,
                                name,
                                url,
                                port,
                                accept_header,
                                wrk_command="wrk"):
    '''
    Generates the string containing the bash script that will be run on the 
    client to benchmark a single test. This specifically works for the variable 
    concurrency tests.
    '''
    headers = _const.headers_template.format(
        server_host=benchmarker_config.server_host, accept=accept_header)
    return _const.concurrency_template.format(
        max_concurrency=max(benchmarker_config.concurrency_levels),
        name=name,
        duration=benchmarker_config.duration,
        levels=" ".join("{}".format(item)
                        for item in benchmarker_config.concurrency_levels),
        server_host=benchmarker_config.server_host,
        port=port,
        url=url,
        headers=headers,
        wrk=wrk_command)


def generate_pipeline_script(benchmarker_config,
                             name,
                             url,
                             port,
                             accept_header,
                             wrk_command="wrk"):
    '''
    Generates the string containing the bash script that will be run on the 
    client to benchmark a single pipeline test.
    '''
    headers = _const.headers_template.format(
        server_host=benchmarker_config.server_host, accept=accept_header)
    return _const.pipeline_template.format(
        max_concurrency=max(benchmarker_config.pipeline_concurrency_levels),
        name=name,
        duration=benchmarker_config.duration,
        levels=" ".join(
            "{}".format(item)
            for item in benchmarker_config.pipeline_concurrency_levels),
        server_host=benchmarker_config.server_host,
        port=port,
        url=url,
        headers=headers,
        wrk=wrk_command,
        pipeline=16)


def generate_query_script(benchmarker_config, name, url, port, accept_header,
                          query_levels):
    '''
    Generates the string containing the bash script that will be run on the 
    client to benchmark a single test. This specifically works for the variable 
    query tests (Query)
    '''
    headers = _const.headers_template.format(
        server_host=benchmarker_config.server_host, accept=accept_header)
    return _const.query_template.format(
        max_concurrency=max(benchmarker_config.concurrency_levels),
        name=name,
        duration=benchmarker_config.duration,
        levels=" ".join("{}".format(item) for item in query_levels),
        server_host=benchmarker_config.server_host,
        port=port,
        url=url,
        headers=headers)


class _const:
    ##########################################################################################
    # Constants
    ##########################################################################################
    headers_template = "-H 'Host: {server_host}' -H 'Accept: {accept}' -H 'Connection: keep-alive'"

    # Used for test types that require no pipelining or query string params.
    concurrency_template = """
      let max_threads=$(cat /proc/cpuinfo | grep processor | wc -l)
      echo ""
      echo "---------------------------------------------------------"
      echo " Running Primer {name}"
      echo " {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}\""
      echo "---------------------------------------------------------"
      echo ""
      {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}"
      sleep 5

      echo ""
      echo "---------------------------------------------------------"
      echo " Running Warmup {name}"
      echo " {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads \"http://{server_host}:{port}{url}\""
      echo "---------------------------------------------------------"
      echo ""
      {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads "http://{server_host}:{port}{url}"
      sleep 5

      echo ""
      echo "---------------------------------------------------------"
      echo " Synchronizing time"
      echo "---------------------------------------------------------"
      echo ""
      ntpdate -s pool.ntp.org

      for c in {levels}
      do
        echo ""
        echo "---------------------------------------------------------"
        echo " Concurrency: $c for {name}"
        echo " {wrk} {headers} --latency -d {duration} -c $c --timeout 8 -t $(($c>$max_threads?$max_threads:$c)) \"http://{server_host}:{port}{url}\""
        echo "---------------------------------------------------------"
        echo ""
        STARTTIME=$(date +"%s")
        {wrk} {headers} --latency -d {duration} -c $c --timeout 8 -t "$(($c>$max_threads?$max_threads:$c))" http://{server_host}:{port}{url}
        echo "STARTTIME $STARTTIME"
        echo "ENDTIME $(date +"%s")"
        sleep 2
      done
    """

    # Used for test types that require pipelining.
    pipeline_template = """
      let max_threads=$(cat /proc/cpuinfo | grep processor | wc -l)
      echo ""
      echo "---------------------------------------------------------"
      echo " Running Primer {name}"
      echo " {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}\""
      echo "---------------------------------------------------------"
      echo ""
      {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}"
      sleep 5

      echo ""
      echo "---------------------------------------------------------"
      echo " Running Warmup {name}"
      echo " {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads \"http://{server_host}:{port}{url}\""
      echo "---------------------------------------------------------"
      echo ""
      {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads "http://{server_host}:{port}{url}"
      sleep 5

      echo ""
      echo "---------------------------------------------------------"
      echo " Synchronizing time"
      echo "---------------------------------------------------------"
      echo ""
      ntpdate -s pool.ntp.org

      for c in {levels}
      do
        echo ""
        echo "---------------------------------------------------------"
        echo " Concurrency: $c for {name}"
        echo " {wrk} {headers} --latency -d {duration} -c $c --timeout 8 -t $(($c>$max_threads?$max_threads:$c)) \"http://{server_host}:{port}{url}\" -s ~/pipeline.lua -- {pipeline}"
        echo "---------------------------------------------------------"
        echo ""
        STARTTIME=$(date +"%s")
        {wrk} {headers} --latency -d {duration} -c $c --timeout 8 -t "$(($c>$max_threads?$max_threads:$c))" http://{server_host}:{port}{url} -s ~/pipeline.lua -- {pipeline}
        echo "STARTTIME $STARTTIME"
        echo "ENDTIME $(date +"%s")"
        sleep 2
      done
    """

    # Used for test types that require a database -
    # These tests run at a static concurrency level and vary the size of
    # the query sent with each request
    query_template = """
      let max_threads=$(cat /proc/cpuinfo | grep processor | wc -l)
      echo ""
      echo "---------------------------------------------------------"
      echo " Running Primer {name}"
      echo " wrk {headers} --latency -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}2\""
      echo "---------------------------------------------------------"
      echo ""
      wrk {headers} --latency -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}2"
      sleep 5

      echo ""
      echo "---------------------------------------------------------"
      echo " Running Warmup {name}"
      echo " wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads \"http://{server_host}:{port}{url}2\""
      echo "---------------------------------------------------------"
      echo ""
      wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads "http://{server_host}:{port}{url}2"
      sleep 5

      echo ""
      echo "---------------------------------------------------------"
      echo " Synchronizing time"
      echo "---------------------------------------------------------"
      echo ""
      ntpdate -s pool.ntp.org

      for c in {levels}
      do
        echo ""
        echo "---------------------------------------------------------"
        echo " Queries: $c for {name}"
        echo " wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads \"http://{server_host}:{port}{url}$c\""
        echo "---------------------------------------------------------"
        echo ""
        STARTTIME=$(date +"%s")
        wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout 8 -t $max_threads "http://{server_host}:{port}{url}$c"
        echo "STARTTIME $STARTTIME"
        echo "ENDTIME $(date +"%s")"
        sleep 2
      done
    """
