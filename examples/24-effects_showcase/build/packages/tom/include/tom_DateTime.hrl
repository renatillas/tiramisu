-record(date_time, {
    date :: gleam@time@calendar:date(),
    time :: gleam@time@calendar:time_of_day(),
    offset :: tom:offset()
}).
