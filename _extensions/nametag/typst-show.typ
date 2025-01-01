#show: doc => sheet(

  $if(info)$
    info: (
    $for(info)$
    (
      line1: [$it.line1$],
      line2: [$it.line2$],
      line3: [$it.line3$],
      line4: [$it.line4$],
    )$sep$,
    $endfor$,
    ),
  $endif$

  $if(logo-left)$
    logo-left: (
      path: "$logo-left.path$"
    ), 
  $endif$

  $if(logo-right)$
    logo-right: (
      path: "$logo-right.path$"
    ), 
  $endif$

  $if(logo-left-height)$
    logo-left-height: $logo-left-height$,
  $endif$

  $if(logo-right-height)$
    logo-right-height: $logo-right-height$,
  $endif$

  $if(font-size)$
    font-size: $font-size$,
  $endif$

  $if(leading)$
    leading: $leading$,
  $endif$

  $if(paper-height)$
    paper-height: $paper-height$,
  $endif$

  $if(paper-width)$
    paper-width: $paper-width$,
  $endif$

  $if(nametag-height)$
    nametag-height: $nametag-height$,
  $endif$

  $if(nametag-width)$
    nametag-width: $nametag-width$,
  $endif$
  
  $if(bg-image)$
    bg-image: (
      path: "$bg-image.path$"
    ), 
  $endif$

  $if(trim-color)$
    trim-color: "$trim-color$",
  $endif$

  $if(text-color)$
    text-color: "$text-color$",
  $endif$

  $if(text-pos-x)$
    text-pos-x: $text-pos-x$,
  $endif$

  $if(text-pos-y)$
    text-pos-y: $text-pos-y$,
  $endif$

  $if(inset)$
    inset: $inset$,
  $endif$
)
