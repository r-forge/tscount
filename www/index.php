
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> More information on the project and the information how to install our R package <tt>tscount</tt> you can find on the <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">project summary page</a> on the software development platform <a href="http://www.r-forge.r-project.org/">R-Forge</a>. You will also find a current development version there, which can be installed by typing <tt>install.packages("tscount", repos="http://R-Forge.R-project.org")</tt> in R. We recommend to use the most recent stable release available from the Comprehensive R Archive Network (<a href="http://cran.r-project.org">CRAN</a>): <a href="http://cran.r-project.org/web/packages/tscount">http://cran.r-project.org/web/packages/tscount</a>.</p>

<p>There is a <a href="http://cran.r-project.org/web/packages/tscount/vignettes/tsglm.pdf">vignette</a> which summarizes the theoretical background of these methods
with references to the literature for further details. It gives details on the implementation
of the package and provides simulation results for models which have not been studied
theoretically before. The usage of the package is demonstrated by two data examples.</p>

<p>We are happy about any kind of feedback on the package! Contributions to the package are very welcome.</p>

<p> <strong>Maintainer:</strong>
<a href="https://www.statistik.tu-dortmund.de/liboschik-en.html">Tobias Liboschik</a>, Department of Statistics, Technische Universit&auml;t Dortmund, Germany.</p>

</body>
</html>
