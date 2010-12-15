# capitalize statement names
#/the .* statement/{
#h
#s/.*the \(.*\) statement.*/\1/
#y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/
#G
#s/\(.*\)\n\(.*the \).*\( statement.*\)/\2\1\3/
#}

/the .* statement/{
h
s/.*the (.*) statement.*/\1/
y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/
G
s/(.*)\n(.*the ).*( statement.*)/\2\1\3/
}
