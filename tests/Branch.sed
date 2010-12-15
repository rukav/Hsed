# Replace the first occurence of a pattern in a whole file
/Administration/{
s/Administration/Supervision/
:loop
n
b loop
}
