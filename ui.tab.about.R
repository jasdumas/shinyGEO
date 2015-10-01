tab.about = tabPanel("About", icon = icon("info-circle"),
    HTML("<div class='panel panel-info'>
  <div class='panel-heading'>
    <h3 class='panel-title'>Directions on Usage</h3>
  </div>
  <div class='panel-body'>
   <ol>
        <li>Choose your GEO Accession Number (GSExxx) which is an original submitter-supplied record that summarizes a study from the 
            <a href = 'http://www.ncbi.nlm.nih.gov/geo/browse/?view=series'>GEO Repository</a></li>
        <li>Select the Platform
        <li>View Expression Profiles and Determine Data Transfomation Method</li>
        <li>View Clinical/Phenotypic Data and Select a Characteristic Column</li>
        <li>Select Gene and Probe</li>
        <li>Select Groups from the Selected Characteristic Column</li>
        <li>Format Differential Expression Analysis Plot'), 
        <li>Select Characteristic Column that contains info on 'Time' to Event</li>
        <li>Select Characteristic Column that contains info on 'Outcome' at Event</li>
        <li>Format 'Time' and 'Outcome' Columns</li>
        <li>View Survival Analysis Kaplan-Meier Plot</li>
        <li>Append Desired Plots to the Reproducible Report</li>
    </ol>
  </div>
</div>"),
                   
    shiny::hr(),  
    HTML("<div class='panel panel-info'>
  <div class='panel-heading'>
         <h3 class='panel-title'>Authors & Contributors</h3>
         </div>
         <div class='panel-body'>
    <span style = \'font-weight: bold\'> Jasmine Dumas </span>
         <p>is a MSc. Graduate Student in the Predictive Analytics program at DePaul University (Chicago, IL/ Online).
         Jasmine has a BSE in Biomedical Engineering from the University of Hartford
         and a Professional Certificate in Medical Product Develpment from UCI Extension.
         <a href ='https://github.com/jasdumas'>Github Profile</a> <span class= \'label label-primary\'>Package Maintainer</span> </p>
         
    <span style = \'font-weight: bold\'> Garrett M. Dancik, PhD </span>
        <p>is an Assistant Professor of Computer Science / Bioinformatics at Eastern
        Connecticut State University (Wilimantic, CT). His research involves applying statistical, mathematical,
        and computational models to answer biological questions and gain insight into biological systems.
        His more recent work has focused on analyzing gene expression data and identifying prognostic tumor biomarkers.
        <a href ='http://bioinformatics.easternct.edu/'>Bioinformatics Laboratory Page</a> <span class= \'label label-primary\'>Package Maintainer</span></p>

   <span style = \'font-weight: bold\'> Ken-Heng Henry Liao </span>
        <p>has BS degrees in Computer Science and Mathematics from Eastern Connecticut State University (Wilimantic, CT). 
        He is currently pursuing a Masters in Bioinformatics at Boston University. <span class= \'label label-info\'>Contributor</span></p>

  <span style = \'font-weight: bold\'> Branden A. Spratt </span>
        <p>is currently pursing a B.S. in Computer Science at Eastern Connecticut State University. <span class= \'label label-info\'>Contributor</span></p>

  <span style = \'font-weight: bold\'> Greg Harper </span>
        <p>is currently studying Computer Science at Eastern Connecticut State University planning to graduate with a B.S. in May 2015. 
        Currently working as a Computer and Network security intern at Cigna until July 2015 at which point he will be starting full time. 
        He has also earned his Craftsman for working Electrical and Enviromental systems on aircraft, as well as having an A.S. in Computer 
        Technology from Manchester Community College. <span class= \'label label-info\'>Contributor</span></p>

  </div>
</div>")
)

