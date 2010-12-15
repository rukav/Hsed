# Remove the HTML tags
/</{
:loop
s/<[^<]*>//g
/</{
N
b loop
}
}
