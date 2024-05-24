# Collaborative Whiteboard

This repository contains the code of our project for Internet Protocols (ELEC-E7320) course at Aalto University. We, as a team of 3, closely worked together to design the protocol, develop the backend and frontend, test, and deploy our project.

## Team Members
- Petteri Pulkkinen
- Sonika Baniya
- Amirhosein Rajabi

## How to run

Use the following command run this project in development mode:

```sh
docker compose up
```

Then visit [http://localhost:8000/](http://localhost:8000/) or [http://localhost:8000/new](http://localhost:8000/new) to create a collaborative whiteboard. 
If you have already created a whiteboard, you can access it at 
[http://localhost:8000/boards/:id](http://localhost:8000/boards/:id), where `:id` is the id of the whiteboard.

Frontend automatically restarts upon changes to source files, but it does not install any new npm packages. 
To install newly-added packages, you need to rebuid the container. You can rebuild the container images with:

```sh
docker compose up --build
```
