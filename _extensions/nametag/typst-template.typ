// functions

// remove escape character
#let remove-escape(str) = {
  str.replace("\\", "")
}

// check if any logo exists
#let validate-logo(ll,lr) = {
  if ll != none or lr != none {
    true
  } else {
    false
  }
}

// text style heavy
#let style-heavy(content) = {
  box(
    text(
      size: 1.2em,
      weight: 600,
      content
    )
  )
}

// text style light
#let style-light(content) = {
  box(
    text(
      size: 0.9em,
      content
    )
  )
}

// main
#let sheet(

  info: (),
  logo-left: none,
  logo-right: none,
  logo-left-height: 5.5mm,
  logo-right-height: 5.5mm,
  font-size: 16pt,
  leading: 0.5em,
  paper-height: 297mm,
  paper-width: 210mm,
  nametag-height: 55mm,
  nametag-width: 90mm,
  bg-image: none,
  trim-color: "#aeb6bf",
  text-color: "#2e4053",
  text-pos-x: 0mm,
  text-pos-y: 0mm,
  inset: 0.3em,
  icon-size: 4.5mm
  
) = {

  let rows = calc.floor(paper-height/nametag-height)
  let cols = calc.floor(paper-width/nametag-width)
  let margin-x = (paper-width - (nametag-width * cols))/2
  let margin-y = (paper-height - (nametag-height * rows))/2
  set page(
    height: paper-height,
    width: paper-width,
    margin: (left: margin-x, right: margin-x, top:margin-y, bottom:margin-y)
  )
  set text(font-size, font: "Lato", fill: rgb(remove-escape(text-color)))
  set par(leading: leading)

  grid(
    columns: cols,
    rows: rows,
    ..info.map(item => {
      let content = (
        if item.line1 != "" and item.line1 != none and item.line1 != [] { 
          style-heavy(item.line1) + "\n" 
        } else { 
          "" 
        }) + (if item.line2 != "" and item.line2 != none and item.line2 != [] { 
          item.line2 + "\n" 
        } else { 
          "" 
        }) + (if item.line3 != "" and item.line3 != none and item.line3 != [] { 
          style-light(item.line3) + "\n" 
        } else { 
          "" 
        }) + (if item.line4 != "" and item.line4 != none and item.line4 != [] { 
          style-light(item.line4) + "\n"
        } else { 
          "" 
        }) + (if item.line5 != "" and item.line5 != none and item.line5 != [] {
          style-light(item.line5)
          if(item.icon != none and item.icon != "") {
            h(2mm)
            box(pad(bottom: -0.2mm, image(item.icon, height: icon-size)))
          }
        });
      
      block(
        fill: if (bg-image != none and bg-image != "") {
          pattern(
            image(bg-image.path, height: nametag-height, width: nametag-width, fit: "cover")
          )
        } else {
          none
        },
        stroke: (paint: rgb(remove-escape(trim-color)), thickness: 0.4pt, dash: "dashed"),
        inset: inset,
        width: nametag-width,
        height: nametag-height,
        breakable: false,
        if validate-logo(logo-left, logo-right) {
          stack(
            dir: ttb,
            grid(
              columns: (1fr, 1fr),
              rows: 1,
              align(left + horizon,
                if (logo-left != none and logo-left != "") {
                  image(logo-left.path, height: logo-left-height)
                }
              ),
              align(right + horizon,
                if (logo-right != none and logo-right != "") {
                  image(logo-right.path, height: logo-right-height)
                }
              ),
            ),
            align(center + horizon, 
              pad(
                left: text-pos-x, 
                top: text-pos-y,
                content
              )
            )
          )
        } else {
          align(center + horizon, 
            pad(
              left: text-pos-x, 
              top: text-pos-y,
              content
            )
          )
        }
      )
    })
  )
}
