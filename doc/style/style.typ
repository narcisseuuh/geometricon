#let std-bibliography = bibliography



#let font-family = ("CMU Serif", "Latin Modern Roman", "New Computer Modern",
                    "Serif")
#let font-family-sans = ("CMU Sans Serif", "Latin Modern Sans",
                         "New Computer Modern Sans", "Sans")
#let font-family-mono = ("Latin Modern Mono", "New Computer Modern Mono",
                         "Mono")

#let font = (
  Large: 17pt,
  footnote: 10pt,
  large: 12pt,
  normal: 10pt,
  script: 8pt,
  small: 9pt,
)

#let affl-keys = ("department", "institution", "location", "country")

#let make-author(author, affls) = {
  let author-affls = if type(author.affl) == array {
    author.affl
  } else {
    (author.affl, )
  }

  let lines = author-affls.map(key => {
    let affl = affls.at(key)
    return affl-keys
      .map(key => affl.at(key, default: none))
      .filter(it => it != none)
      .join("\n")
  }).map(it => emph(it))

  return block(spacing: 0em, {
    set par(justify: true, leading: 0.50em)  
    set par(spacing: 0em)
    text(size: font.normal)[*#author.name*\ ]
    text(size: font.normal)[#lines.join([\ ])]
  })
}

#let make-authors(authors, affls) = {
  let cells = authors
    .map(it => (make-author(it, affls)))
    .join()
  return grid(
    columns: (2fr, 1fr),
    align: (left + top, right + top),
    row-gutter: 15.8pt,  
    cells)
}

#let make-title(title, authors, abstract, review, accepted) = {
  
  v(-0.03in)  
  block(spacing: 0em, {
    set block(spacing: 0em)
    set par(leading: 10pt)  
    text(font: font-family, size: font.Large, weight: "bold", title)
  })

  v(31pt, weak: true)  
  make-authors(..authors)
  v(-2pt)  
  v(0.45in, weak: true)  

  
  block(spacing: 0em, width: 100%, {
    set text(size: font.normal)
    set par(leading: 0.51em)  

    
    align(center,
      text(
        font: font-family-sans,
        size: font.large,
        weight: "bold",
        [*Abstract*]))
    v(22.2pt, weak: true)
    pad(left: 0.5in, right: 0.5in, abstract)
  })
  v(29.5pt, weak: true)  
}

#let temp(
  title: [],
  authors: (),
  keywords: (),
  date: auto,
  abstract: none,
  bibliography: none,
  appendix: none,
  accepted: false,
  review: none,
  pubdate: none,
  body,
) = {
  if pubdate == none {
    pubdate = if date != auto and data != none {
      date
    } else {
      datetime.today()
    }
  }

  
  let author = if accepted == none or accepted {
    authors.at(0).map(it => it.name)
  } else {
    ()
  }

  set document(
    title: title,
    author: author,
    keywords: keywords,
    date: date)

  set page(
    paper: "us-letter",
    margin: (left: 1in,
             right: 8.5in - (1in + 6.5in),
             
             
             top: 1.18in,
             bottom: 11in - (1.18in + 9in)),
  )

  
  
  set text(font: font-family, size: font.normal)
  set par(justify: true, leading: 0.52em)  
  show par: set par(spacing: 1.1em)

  
  set heading(numbering: "1.1")
  show heading: set text(font: font-family-sans)
  show heading: it => {
    
    let number = if it.numbering != none {
      counter(heading).display(it.numbering)
    }

    
    let unnumbered = (
      [Broader Impact Statement],
      [Author Contributions],
      [Acknowledgments],
    )
    let level = it.level
    let prefix = [#number ]
    if unnumbered.any(name => name == it.body) {
      level = 3
      prefix = []
    }

    
    set align(left)
    if level == 1 {
      text(size: font.large, weight: "bold", {
        let ex = 10pt
        v(2.05 * ex, weak: true)  
        [#prefix*#it.body*]
        v(1.80 * ex, weak: true) 
      })
    } else if level == 2 {
      text(size: font.normal, weight: "bold", {
        let ex = 6.78pt
        v(2.8 * ex, weak: true)  
        [#prefix*#it.body*]
        v(2.15 * ex, weak: true)  
      })
    } else if level == 3 {
      text(size: font.normal, weight: "bold", {
        let ex = 6.78pt
        v(2.7 * ex, weak: true)  
        [#prefix*#it.body*]
        v(2.0 * ex, weak: true)  
      })
    }
  }

  
  show raw: set block(spacing: 1.95em)

  
  show footnote.entry: set text(size: 8pt)
  set footnote.entry(
    separator: line(length: 2in, stroke: 0.35pt),
    clearance: 6.65pt,
    gap: 0.40em,
    indent: 12pt)  

  
  show figure.caption: set align(left)

  
  show figure.where(kind: image): set figure.caption(position: bottom)
  set figure(gap: 16pt)

  
  show figure.where(kind: table): set figure.caption(position: top)
  show figure.where(kind: table): set figure(gap: 6pt)
  set table(inset: 4pt)

  
  set enum(indent: 2.4em, spacing: 1.3em)
  show enum: set block(above: 2em)

  
  set list(indent: 2.4em, spacing: 1.3em, marker: ([•], [‣], [⁃]))
  show list: set block(above: 2em)

  
  set math.equation(numbering: "(1)", supplement: [])
  show ref: it => {
    let eq = math.equation
    let el = it.element
    if el != none and el.func() == eq {
      let numb = numbering(
        "1",
        ..counter(eq).at(el.location())
      )
      let color = rgb(0%, 8%, 45%)  
      let content = link(el.location(), text(fill: color, numb))
      [(#content)]
    } else {
      it
    }
  }

  
  make-title(title, authors, abstract, review, accepted)
  
  body

  if bibliography != none {
    set std-bibliography(title: [References], style: "style.csl")
    bibliography
  }

  if appendix != none {
    set heading(numbering: "A.1")
    counter(heading).update(0)
    appendix
  }
}
