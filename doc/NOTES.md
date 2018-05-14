
Notes
=====

These notes are "lessons" learned from the BasicLEO flight. This information should be brought
forward as we design and implement the ion drive demonstration system.

+ Including time-outs on these bus communications is essential. We don't want the system getting
  hung waiting indefinitely for communications that never occur.

+ Don't use the main software loop as a timing mechanism. It is inaccurate, of course, but more
  than that it causes problems if the loop is heavily delayed due to time-outs (see above).
  Instead some kind of real-time clock hardware should be used.

+ Don't underestimate the importance of minimizing writes to SD cards. Frequent error logs, for
  example, can "quickly" consume all the writes the card can withstand causing premature
  failures. Ideally some kind of estimate should be done of minimum useful life of an SD card
  based on write activity and that life should be compared to the expected duration of the
  mission.

