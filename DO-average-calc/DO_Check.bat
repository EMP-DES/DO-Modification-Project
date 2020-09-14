@echo off
echo Running DO Code . . . 
Rscript "S:\M & A BRANCH\Discrete EMP\Code\DO-average-calc\DO_avg_script.R"
if NOT ["%errorlevel%"]==["0"] pause