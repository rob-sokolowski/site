module QueryBuilderTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import QueryBuilder exposing (Aggregation(..), Granularity(..), KimballColumn(..), TableRef, TimeClass(..), queryBuilder)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "QueryBuilder"
        [ describe "Simple DuckDB SQL queries from `List KimballColumn`"
            [ test "one dimension"
                (\_ -> queryBuilder [ dim1 ] tRef |> Expect.equal "select dim1 from a_table")
            , test "two dimensions"
                (\_ -> queryBuilder [ dim1, dim2 ] tRef |> Expect.equal "select dim1, dim2 from a_table")
            , test "one time"
                (\_ -> queryBuilder [ time1 ] tRef |> Expect.equal "select time1 from a_table")
            , test "two times"
                (\_ -> queryBuilder [ time1, time2 ] tRef |> Expect.equal "select time1, date_trunc('month', time2) from a_table")
            , test "one measure agg"
                (\_ -> queryBuilder [ measure1 ] tRef |> Expect.equal "select sum(measure1) from a_table")
            , test "two measure aggs"
                (\_ -> queryBuilder [ measure1, measure2 ] tRef |> Expect.equal "select sum(measure1), avg(measure2) from a_table")
            , test "one dimension, one time"
                (\_ -> queryBuilder [ dim1, time1 ] tRef |> Expect.equal "select dim1, time1 from a_table")
            , test "one dimension, one measure agg"
                (\_ -> queryBuilder [ dim1, measure1 ] tRef |> Expect.equal "select dim1, sum(measure1) from a_table group by 1")
            , test "one time, one measure agg"
                (\_ -> queryBuilder [ time1, measure1 ] tRef |> Expect.equal "select time1, sum(measure1) from a_table group by 1")
            , test "one dimension, one time, one measure agg"
                (\_ -> queryBuilder [ dim1, measure1, time1 ] tRef |> Expect.equal "select dim1, time1, sum(measure1) from a_table group by 1, 2")
            ]
        ]


tRef : TableRef
tRef =
    "a_table"


dim1 : KimballColumn
dim1 =
    Dimension "dim1"


dim2 : KimballColumn
dim2 =
    Dimension "dim2"


time1 : KimballColumn
time1 =
    Time Continuous "time1"


time2 : KimballColumn
time2 =
    Time (Discrete Month) "time2"


measure1 : KimballColumn
measure1 =
    Measure Sum "measure1"


measure2 : KimballColumn
measure2 =
    Measure Mean "measure2"
