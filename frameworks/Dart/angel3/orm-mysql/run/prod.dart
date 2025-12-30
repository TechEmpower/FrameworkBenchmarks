import 'package:angel3_container/mirrors.dart';
import 'package:angel3_production/angel3_production.dart';
import 'package:orm_mysql_app/benchmark_app.dart';

void main(List<String> args) =>
    Runner('Angel3', configureServer, reflector: MirrorsReflector()).run(args);
