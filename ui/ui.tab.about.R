tab.about = tabItem("About",
    HTML("<div class='panel panel-default'>
  <div class='panel-heading'>
         <h3 class='panel-title' style = \'font-weight: bold\'>Citation</h3>
   </div>
   <div class='panel-body'>
   <p> Dumas J, Gargano MA, Dancik GM. <i>shinyGEO</i>: a web-based application for analyzing Gene Expression Omnibus datasets. Bioinformatics. 2016 Dec 1;32(23):3679-3681 </p>
   <p> For update-to-date information, see the homepage: <a href = 'http://gdancik.github.io/shinyGEO/' target = \'_blank\'> http://gdancik.github.io/shinyGEO/ </a> </p> 
   </div>
  <div class='panel-heading'>
         <h3 class='panel-title' style = \'font-weight: bold\'>Primary Contributors</h3>
   </div>
   <div class='panel-body'>
    <p><span style = \'font-weight: bold\'> Garrett M. Dancik, PhD </span>
        is an Associate Professor of Computer Science / <a href = 'http://bioinformatics.easternct.edu'>Bioinformatics</a> at Eastern
        Connecticut State University (Wilimantic, CT). <span class= \'label label-primary\'>Package Maintainer</span></p>

    <p> <span style = \'font-weight: bold\'> Jasmine Dumas </span>
         is a MSc. Graduate Student in the Predictive Analytics program at DePaul University (Chicago, IL/ Online).
         Jasmine has a BSE in Biomedical Engineering from the University of Hartford
         and a Professional Certificate in Medical Product Develpment from UCI Extension.
         <a href ='http://jasdumas.github.io/'>Jasmine's Homepage</a> <span class= \'label label-primary\'>Contributor</span> </p>
    <p><span style = \'font-weight: bold\'> Michael Gargano </span>
        has a BS degree in Computer Science from Eastern Connecticut State University. He is pursuing a Masters in Bioinformatics at Northeastern University.<span class=\'label label-primary\'>Contributor</span></p> 
  </div>

  <div class='panel-heading'>
         <h3 class='panel-title' style = \'font-weight: bold\'>Additional Contributors</h3>
</div>
<div class = 'panel-body'>
<p>   <span style = \'font-weight: bold\'> Ken-Heng Henry Liao </span>
        has BS degrees in Computer Science and Mathematics from Eastern Connecticut State University (Wilimantic, CT). 
        He is currently pursuing a Masters in Bioinformatics at Boston University. <span class= \'label label-info\'>Contributor</span></p>

  <p><span style = \'font-weight: bold\'> Branden A. Spratt </span>
        has a B.S. in Computer Science from Eastern Connecticut State University. <span class= \'label label-info\'>Contributor</span></p>

  <p><span style = \'font-weight: bold\'> Greg Harper </span>
        has a B.S. in  Computer Science from Eastern Connecticut State University. 
        He is currently working as a Computer and Network security intern at Cigna. 
        He has also earned his Craftsman for working Electrical and Enviromental systems on aircraft, as well as having an A.S. in Computer 
        Technology from Manchester Community College. <span class= \'label label-info\'>Contributor</span></p>

  </div>
</div>"),
    tags$h4("Session Information"),
    verbatimTextOutput("test")
)

