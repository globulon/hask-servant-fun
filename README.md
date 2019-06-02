# Trigger practice on Haskell servant 

Canonical example of the user REST API service 
in order to discover Haskell servant

## Check List

- [x] Implement base example
- [x] Add in memory cache using IORef
- [x] Separate concerns
- [x] Make handlers more visible
- [ ] Create User
- [ ] Delete User
- [ ] Generate IDs for management
- [ ] Manages status codes versus _"business domain"_ behaviour
- [ ] separate error management

## Usage

Make sure you have stack installed with eversion 1.9.+

after applying 

```bash
stack setup
```

execute with 

```bash
stack run
```

##### Get all users
```bash
curl http://localhost:8080/users
```

##### Get users by name (waiting for ID)
```bash
curl http://localhost:8080/user/isaac
```

##### create a new user
```bash
curl -d '{"email":"globulon@gmail.com","registration_date":"1971-08-28","age":47,"name":"OMD"}' -H "Content-Type: application/json" -X POST http://localhost:8080/user
```