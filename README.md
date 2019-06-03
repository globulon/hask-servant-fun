# Trigger practice on Haskell servant 

Canonical example of the user REST API service 
in order to discover Haskell servant

## Check List

- [x] Implement base example
- [x] Add in memory cache using IORef
- [x] Separate concerns
- [x] Make handlers more visible
- [x] Create User
- [x] Delete User
- [ ] Generate IDs for management
- [ ] Makes status codes more meaningful 
- [ ] separate error management

## Usage

Make sure you have stack installed with version 1.9.+

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
curl -d '{"email":"globulon@gmail.com","registration_date":"1971-08-28", "name":"omd"}' -H "Content-Type: application/json" -X POST http://localhost:8080/user
```

##### delete a user
```bash
curl -v -X DELETE localhost:8080/user/andrea
```