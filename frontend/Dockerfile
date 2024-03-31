FROM node:20-alpine

WORKDIR /app
COPY package*.json ./

RUN npm install -g create-react-app

EXPOSE 3000

CMD npm start
