IMAGENAME=reseptilaakesovellus
sudo docker build -t $IMAGENAME .
sudo docker run --env-file .Renviron $IMAGENAME
