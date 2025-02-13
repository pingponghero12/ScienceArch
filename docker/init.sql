-- Create Tables
CREATE TABLE READ_STATES (
  state VARCHAR(50) PRIMARY KEY
);

CREATE TABLE GENRES (
  genre VARCHAR(50) PRIMARY KEY
);

CREATE TABLE HASHTAGS (
  hashtag VARCHAR(50) PRIMARY KEY
);

CREATE TABLE AUTHOR_STATUS (
  author_status VARCHAR(50) PRIMARY KEY
);

CREATE TABLE USERS (
  user_id INT AUTO_INCREMENT PRIMARY KEY,
  admin TINYINT(1) DEFAULT 0,
  username VARCHAR(100) NOT NULL,
  description TEXT,
  image VARCHAR(255),
  `read` INT DEFAULT 0,
  contributions INT DEFAULT 0,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  last_login DATETIME,
  INDEX (username)
);

CREATE TABLE USER_CREDENTIALS (
  user_id INT PRIMARY KEY,
  email VARCHAR(255) NOT NULL,
  password_hash VARCHAR(255) NOT NULL,
  FOREIGN KEY (user_id) REFERENCES USERS(user_id)
);

CREATE TABLE FOLLOW (
  follower_id INT NOT NULL,
  user_id INT NOT NULL,
  PRIMARY KEY (follower_id, user_id),
  FOREIGN KEY (follower_id) REFERENCES USERS(user_id),
  FOREIGN KEY (user_id) REFERENCES USERS(user_id)
);

CREATE TABLE AUTHORS (
  author_id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  details TEXT,
  image VARCHAR(255),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE PAPERS (
  paper_id INT AUTO_INCREMENT PRIMARY KEY,
  title VARCHAR(255) NOT NULL,
  description TEXT,
  format VARCHAR(100),
  avg_score INT DEFAULT 0,
  popularity INT DEFAULT 0,
  original_title VARCHAR(255),
  published_at DATETIME,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE POSTS (
  post_id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT NOT NULL,
  title VARCHAR(255),
  content TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES USERS(user_id)
);

CREATE TABLE ACTIVITIES (
  activity_id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT NOT NULL,
  read_state VARCHAR(50),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES USERS(user_id),
  FOREIGN KEY (read_state) REFERENCES READ_STATES(state)
);

CREATE TABLE PAPERS_USER (
  paper_id INT NOT NULL,
  user_id INT NOT NULL,
  read_state VARCHAR(50),
  rating INT DEFAULT 0,
  times_read INT DEFAULT 0,
  PRIMARY KEY (paper_id, user_id),
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (user_id) REFERENCES USERS(user_id),
  FOREIGN KEY (read_state) REFERENCES READ_STATES(state)
);

CREATE TABLE PAPER_CITATIONS (
  paper_id INT NOT NULL,
  citation_id INT NOT NULL,
  PRIMARY KEY (paper_id, citation_id),
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (citation_id) REFERENCES PAPERS(paper_id)
);

CREATE TABLE PAPER_CITES (
  paper_id INT NOT NULL,
  cites_id INT NOT NULL,
  PRIMARY KEY (paper_id, cites_id),
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (cites_id) REFERENCES PAPERS(paper_id)
);

CREATE TABLE PAPER_SIMILAR (
  paper_id INT NOT NULL,
  similar_id INT NOT NULL,
  PRIMARY KEY (paper_id, similar_id),
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (similar_id) REFERENCES PAPERS(paper_id)
);

CREATE TABLE PAPERS_GENRES (
  paper_id INT NOT NULL,
  genre VARCHAR(50) NOT NULL,
  PRIMARY KEY (paper_id, genre),
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (genre) REFERENCES GENRES(genre)
);

CREATE TABLE PAPERS_HASHTAGS (
  paper_id INT NOT NULL,
  hashtag VARCHAR(50) NOT NULL,
  PRIMARY KEY (paper_id, hashtag),
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (hashtag) REFERENCES HASHTAGS(hashtag)
);

CREATE TABLE PAPER_SUBMISSIONS (
  submission_id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT NOT NULL,
  title VARCHAR(255),
  description TEXT,
  format VARCHAR(100),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  accepted TINYINT(1) NOT NULL DEFAULT 0,
  FOREIGN KEY (user_id) REFERENCES USERS(user_id)
);

CREATE TABLE AUTHOR_SUBMISSIONS (
  author_submission_id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255),
  details TEXT,
  image VARCHAR(255),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  accepted TINYINT(1) NOT NULL DEFAULT 0
);

CREATE TABLE AUTHOR_REVISIONS (
  author_revision_id INT AUTO_INCREMENT PRIMARY KEY,
  author_id INT NOT NULL,
  name VARCHAR(255),
  details TEXT,
  image VARCHAR(255),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  accepted TINYINT(1) NOT NULL DEFAULT 0,
  FOREIGN KEY (author_id) REFERENCES AUTHORS(author_id)
);

CREATE TABLE AUTHOR_PAPER (
  author_id INT NOT NULL,
  paper_id INT NOT NULL,
  author_status VARCHAR(50),
  PRIMARY KEY (author_id, paper_id),
  FOREIGN KEY (author_id) REFERENCES AUTHORS(author_id),
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (author_status) REFERENCES AUTHOR_STATUS(author_status)
);

CREATE TABLE PAPER_REVISIONS (
  revision_id INT AUTO_INCREMENT PRIMARY KEY,
  paper_id INT NOT NULL,
  user_id INT NOT NULL,
  title VARCHAR(255),
  description TEXT,
  format VARCHAR(100),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  accepted TINYINT(1) NOT NULL DEFAULT 0,
  FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
  FOREIGN KEY (user_id) REFERENCES USERS(user_id)
);

CREATE TABLE REVISION_GENRES (
  revision_id INT NOT NULL,
  genre VARCHAR(50) NOT NULL,
  PRIMARY KEY (revision_id, genre),
  FOREIGN KEY (revision_id) REFERENCES PAPER_REVISIONS(revision_id),
  FOREIGN KEY (genre) REFERENCES GENRES(genre)
);

CREATE TABLE REVISION_HASHTAGS (
  revision_id INT NOT NULL,
  hashtag VARCHAR(50) NOT NULL,
  PRIMARY KEY (revision_id, hashtag),
  FOREIGN KEY (revision_id) REFERENCES PAPER_REVISIONS(revision_id),
  FOREIGN KEY (hashtag) REFERENCES HASHTAGS(hashtag)
);

-- TRIGGERS

DELIMITER $$

CREATE TRIGGER PAPER_SUBMISSION_INSERT_ADD_READ
AFTER INSERT ON PAPERS_USER
FOR EACH ROW
BEGIN
  IF NEW.read_state <> 'Plan to read' THEN
    UPDATE USERS
    SET `read` = `read` + 1
    WHERE user_id = NEW.user_id;
  END IF;
END$$

-- Trigger for updates in PAPERS_USER
CREATE TRIGGER PAPER_SUBMISSION_UPDATE_ADD_READ
AFTER UPDATE ON PAPERS_USER
FOR EACH ROW
BEGIN
  IF OLD.read_state = 'Plan to read' AND NEW.read_state <> 'Plan to read' THEN
    UPDATE USERS
    SET `read` = `read` + 1
    WHERE user_id = NEW.user_id;
  END IF;
END$$

-- TRIGGER: PAPER_SUBMISSION_ACCEPTED_ADD_CONTRIBUTION
CREATE TRIGGER PAPER_SUBMISSION_ACCEPTED_ADD_CONTRIBUTION
AFTER UPDATE ON PAPER_SUBMISSIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    UPDATE USERS
    SET contributions = contributions + 1
    WHERE user_id = NEW.user_id;
  END IF;
END$$

CREATE TRIGGER PAPER_SUBMISSION_ACCEPTED_ADD_PAPER
AFTER UPDATE ON PAPER_SUBMISSIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    INSERT INTO PAPERS (
      title,
      description,
      format,
      original_title,
      published_at
    )
    VALUES (
      NEW.title,
      NEW.description,
      NEW.format,
      NEW.title,
      CURRENT_TIMESTAMP
    );
  END IF;
END$$

-- TRIGGER: PAPER_REVISION_ACCEPTED_ADD_CONTRIBUTION
CREATE TRIGGER PAPER_REVISION_ACCEPTED_ADD_CONTRIBUTION
AFTER UPDATE ON PAPER_REVISIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    UPDATE USERS
    SET contributions = contributions + 1
    WHERE user_id = NEW.user_id;
  END IF;
END$$

-- TRIGGER: AUTHOR_SUBMISSION_ACCEPTED_ADD_CONTRIBUTION
CREATE TRIGGER AUTHOR_SUBMISSION_ACCEPTED_ADD_CONTRIBUTION
AFTER UPDATE ON AUTHOR_SUBMISSIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    UPDATE USERS
    SET contributions = contributions + 1
    WHERE user_id = 999999999;
  END IF;
END$$

-- TRIGGER: AUTHOR_REVISION_ACCEPTED_ADD_CONTRIBUTION
CREATE TRIGGER AUTHOR_REVISION_ACCEPTED_ADD_CONTRIBUTION
AFTER UPDATE ON AUTHOR_REVISIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    UPDATE USERS
    SET contributions = contributions + 1
    WHERE user_id = 999999999;
  END IF;
END$$

-- TRIGGER: PAPERS_USER_INSERT_ACTIVITY
CREATE TRIGGER PAPERS_USER_INSERT_ACTIVITY
AFTER INSERT ON PAPERS_USER
FOR EACH ROW
BEGIN
  INSERT INTO ACTIVITIES (user_id, read_state, created_at)
  VALUES (NEW.user_id, NEW.read_state, CURRENT_TIMESTAMP);
END$$

-- TRIGGER: PAPERS_USER_UPDATE_ACTIVITY
CREATE TRIGGER PAPERS_USER_UPDATE_ACTIVITY
AFTER UPDATE ON PAPERS_USER
FOR EACH ROW
BEGIN
  INSERT INTO ACTIVITIES (user_id, read_state, created_at)
  VALUES (NEW.user_id, NEW.read_state, CURRENT_TIMESTAMP);
END$$

-- TRIGGER: POST_UPDATE_TIMESTAMP
CREATE TRIGGER POST_UPDATE_TIMESTAMP
BEFORE UPDATE ON POSTS
FOR EACH ROW
BEGIN
  SET NEW.updated_at = CURRENT_TIMESTAMP;
END$$

-- TRIGGER: PAPER_REVISION_ACCEPTED_UPDATE_PAPER
CREATE TRIGGER PAPER_REVISION_ACCEPTED_UPDATE_PAPER
AFTER UPDATE ON PAPER_REVISIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    DELETE FROM PAPERS_GENRES WHERE paper_id = NEW.paper_id;
    DELETE FROM PAPERS_HASHTAGS WHERE paper_id = NEW.paper_id;
    INSERT INTO PAPERS_GENRES (paper_id, genre)
      SELECT NEW.paper_id, rg.genre
      FROM REVISION_GENRES rg
      WHERE rg.revision_id = NEW.revision_id;
    INSERT INTO PAPERS_HASHTAGS (paper_id, hashtag)
      SELECT NEW.paper_id, rh.hashtag
      FROM REVISION_HASHTAGS rh
      WHERE rh.revision_id = NEW.revision_id;
  END IF;
END$$

-- TRIGGER: PAPER_DELETE_CLEAR_REFERENCES
CREATE TRIGGER PAPER_DELETE_CLEAR_REFERENCES
AFTER DELETE ON PAPERS
FOR EACH ROW
BEGIN
  DELETE FROM PAPERS_USER WHERE paper_id = OLD.paper_id;
  DELETE FROM AUTHOR_PAPER WHERE paper_id = OLD.paper_id;
END$$

-- TRIGGER: USER_DELETE_CLEAR_REFERENCES
CREATE TRIGGER USER_DELETE_CLEAR_REFERENCES
AFTER DELETE ON USERS
FOR EACH ROW
BEGIN
  DELETE FROM ACTIVITIES WHERE user_id = OLD.user_id;
  DELETE FROM PAPERS_USER WHERE user_id = OLD.user_id;
  DELETE FROM FOLLOW WHERE user_id = OLD.user_id OR follower_id = OLD.user_id;
END$$

-- TRIGGER: AUTHOR_SUBMISSION_ACCEPTED_CREATE_AUTHOR
CREATE TRIGGER AUTHOR_SUBMISSION_ACCEPTED_CREATE_AUTHOR
AFTER UPDATE ON AUTHOR_SUBMISSIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    INSERT INTO AUTHORS (name, details, image, created_at, updated_at)
    VALUES (NEW.name, NEW.details, NEW.image, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
  END IF;
END$$

-- TRIGGER: AUTHOR_REVISION_ACCEPTED_UPDATE_AUTHOR
CREATE TRIGGER AUTHOR_REVISION_ACCEPTED_UPDATE_AUTHOR
AFTER UPDATE ON AUTHOR_REVISIONS
FOR EACH ROW
BEGIN
  IF OLD.accepted = 0 AND NEW.accepted = 1 THEN
    UPDATE AUTHORS
    SET name = NEW.name, details = NEW.details, image = NEW.image, updated_at = CURRENT_TIMESTAMP
    WHERE author_id = NEW.author_id;
  END IF;
END$$

DELIMITER ;

-- ADD USER
DELIMITER $$

CREATE PROCEDURE CreateUserProcedure(
    IN p_email VARCHAR(255),
    IN p_password_hash VARCHAR(255),
    IN p_username VARCHAR(100)
)
BEGIN
    DECLARE exit HANDLER FOR SQLEXCEPTION
    BEGIN
        ROLLBACK;
    END;

    START TRANSACTION;

    INSERT INTO USERS (
        username,
        description,
        image,
        `read`,
        contributions,
        created_at,
        last_login
    ) VALUES (
        p_username,
        NULL,
        NULL,
        0,
        0,
        CURRENT_TIMESTAMP,
        NULL
    );

    INSERT INTO USER_CREDENTIALS (
        user_id,
        email,
        password_hash
    ) VALUES (
        LAST_INSERT_ID(),
        p_email,
        BCRYPT(p_password_hash, 256)
    );

    COMMIT;
END $$

DELIMITER ;

--- Authorization
DELIMITER $$

CREATE PROCEDURE AuthorizationProcedure(
    IN p_email VARCHAR(255),
    IN p_password VARCHAR(255),
    OUT p_user_id INT,
    OUT p_username VARCHAR(100),
    OUT p_admin TINYINT(1)
)
BEGIN
  set p_user_id = 0;
  set p_username = "";
  set p_admin = -1;

  SELECT user_id INTO p_user_id 
    FROM USER_CREDENTIALS
    WHERE email = p_email 
    AND password_hash = BCRYPT(p_password, 256) 
    LIMIT 1;

  SELECT username, admin INTO p_username, p_admin
    FROM USERS
    WHERE user_id = p_user_id;

END $$

DELIMITER ;

CREATE VIEW AllUsers AS
SELECT
    u.user_id,
    u.username
FROM USERS u;


-- Create users
CREATE USER 'notlog_user'@'%' IDENTIFIED BY 'notlog1234';
GRANT SELECT ON papers_db.AllUsers TO 'notlog_user'@'%';
GRANT EXECUTE ON PROCEDURE papers_db.CreateUserProcedure TO 'notlog_user'@'%';
GRANT EXECUTE ON PROCEDURE papers_db.AuthorizationProcedure TO 'notlog_user'@'%';
FLUSH PRIVILEGES;

CREATE USER 'normal_user'@'%' IDENTIFIED BY 'normal1234';
GRANT SELECT ON papers_db.AllUsers TO 'normal_user'@'%';
GRANT EXECUTE ON PROCEDURE papers_db.CreateUserProcedure TO 'normal_user'@'%';
GRANT EXECUTE ON PROCEDURE papers_db.AuthorizationProcedure TO 'normal_user'@'%';
FLUSH PRIVILEGES;

CREATE USER 'super_user'@'%' IDENTIFIED BY 'super1234';
GRANT SELECT ON papers_db.AllUsers TO 'super_user'@'%';
GRANT EXECUTE ON PROCEDURE papers_db.CreateUserProcedure TO 'super_user'@'%';
GRANT EXECUTE ON PROCEDURE papers_db.AuthorizationProcedure TO 'super_user'@'%';
FLUSH PRIVILEGES;


