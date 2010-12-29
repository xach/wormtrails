Wormtrails

The general idea is this:

  - a CHART object contains a bunch of BUCKETs and knows about a bunch
    of THINGs

  - a BUCKET holds a number of SAMPLEs. A bucket might represent a
    weekend (for box offices), an hour (for measuring samples across a
    24 hour period), a day, etc. A bucket is identified within a chart
    by a numerical index. For example, the box office buckets are
    identified by indexes of the form 200801, meaning the first
    weekend of 2008. Hourly buckets are indexed from 0 to 23. BUCKETs
    with the same index are EQ.

  - a THING represents a discrete item being tracked over time. For
    the IRC charts, that's a nickname. For the movie charts, that's a
    movie. The THING's name is used as a unique identifier; all THINGs
    with the same name are EQ.

  - a SAMPLE is the aggregate value of a THING in a BUCKET. The value
    might represent a simple count, as in the IRC and referer charts,
    or the monetary gross, as in the movie charts. Before layout, a
    SAMPLE is 

The CHART is the top-level data structure. The steps are generally:

  - create a CHART (or subclass) object

  - use ADD-DATA for a simple way to create and accumulate BUCKET,
    THING, and SAMPLE objects into the CHART

  - lay out the CHART with LAYOUT; this establishes the rectangles
    associated with SAMPLEs and their relative positions on an
    abstract canvas

Drawing with vecto:

  - draw the labels and grid for the entire CHART

  - draw the rectangles for each SAMPLE into the PNG and add
    connecting glue

    - the colors for each samplebox/glue are taken from the color of
      the THING. the colors of each thing are initialized with
      ESTABLISH-COLORS. See RAINBOW-COLOR-MIXIN in wormtrails.lisp to
      see an example of how to set colors up by the position of the
      debut bucket for a thing, or irc.lisp to see how to set colors
      of a thing based on which bucket the max sample value falls in.

  - draw the labels for each THING with DRAW-LABEL. The default method
    does that by drawing the CHART-LABEL of the THING into the
    SCREENBOX of an appropriate SAMPLE for the THING, as found with
    BEST-LABEL-SAMPLE

    - the default method for BEST-LABEL-SAMPLE just returns first
      sample that would fit the label, but it can be specialized. The
      IRC example changes things to use the sample with the maximum
      value

    - (samples thing) returns a list of all samples for a THING, and
      could be used for some other criteria to select one to draw

  - draw the labels for each BUCKET with DRAW-LABEL; the default
    method just draws the CHART-LABEL of the BUCKET beneath the
    bounding box of the bucket

    - CHART-LABEL for a BUCKET could be specialized to produce a
      prettier string than just the numerical index; for example the
      IRC hours are converted to HH:00, with MIDNIGHT and NOON
      returned for 0 and 12 respectively


Scaling is important to get a chart that looks pretty. I haven't
determined any automatic way to do scaling yet; I just fiddle with
values. Using consistent scaling also makes it much easier to compare
chart-to-chart.

The imagemap in this version is somehwat primitive. You will need to
change the link and onmouseover actions in WRITE-IMAGEMAP-AREA to do
something useful if you want to use the PNG/imagemap capability. 


I think irc.lisp, referers.lisp, and pastes.lisp are good ways to
figure out how to get started with simple stuff. More complex stuff,
just let me know.

