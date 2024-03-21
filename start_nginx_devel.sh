docker run --rm --name nginx-devel --network host -v ./nginx-development.conf:/etc/nginx/nginx.conf:ro -it nginx
