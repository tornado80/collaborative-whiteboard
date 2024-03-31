FROM node:18

WORKDIR /app
COPY package*.json ./

RUN npm install -g create-react-app
RUN npm install

EXPOSE 3000

CMD npm start
