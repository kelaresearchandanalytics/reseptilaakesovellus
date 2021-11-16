IMAGENAME=korona_atc_app
sudo docker build -t $IMAGENAME .
sudo docker run --env-file .Renviron $IMAGENAME
