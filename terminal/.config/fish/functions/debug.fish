function debug -d "Debug a program with gdb if it crashes"
    # https://stackoverflow.com/questions/8657648
    gdb -ex='set confirm on' -ex=run -ex=quit --args $argv
end
