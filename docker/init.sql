-- Create users
CREATE USER 'normal_user'@'%' IDENTIFIED BY 'some_strong_password';
FLUSH PRIVILEGES;

CREATE USER 'super_user'@'%' IDENTIFIED BY 'some_strong_password';
FLUSH PRIVILEGES;

CREATE TABLE USERS (
    user_id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(255) NOT NULL,
    description TEXT,
    image VARCHAR(255),
    read_count INT DEFAULT 0,
    contributions INT DEFAULT 0,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    last_login DATETIME
);

CREATE TABLE USER_CREDENTIALS (
    credential_id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
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
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    accepted BOOLEAN DEFAULT FALSE
);

CREATE TABLE READ_STATES (
    state VARCHAR(50) PRIMARY KEY
);

CREATE TABLE ACTIVITIES (
    activity_id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    read_state VARCHAR(50),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES USERS(user_id),
    FOREIGN KEY (read_state) REFERENCES READ_STATES(state)
);

CREATE TABLE POSTS (
    post_id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    title VARCHAR(255) NOT NULL,
    content TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES USERS(user_id)
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
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    accepted BOOLEAN DEFAULT FALSE
);

CREATE TABLE PAPERS_USER (
    paper_id INT NOT NULL,
    user_id INT NOT NULL,
    read_on DATETIME DEFAULT CURRENT_TIMESTAMP,
    read_state VARCHAR(50),
    rating INT,
    PRIMARY KEY (paper_id, user_id),
    FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
    FOREIGN KEY (user_id) REFERENCES USERS(user_id),
    FOREIGN KEY (read_state) REFERENCES READ_STATES(state)
);

CREATE TABLE GENRES (
    genre VARCHAR(50) PRIMARY KEY
);

CREATE TABLE HASHTAGS (
    hashtag VARCHAR(50) PRIMARY KEY
);

CREATE TABLE PAPERS_GENRES (
    id INT AUTO_INCREMENT PRIMARY KEY,
    paper_id INT NOT NULL,
    genre VARCHAR(50) NOT NULL,
    FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
    FOREIGN KEY (genre) REFERENCES GENRES(genre)
);

CREATE TABLE PAPERS_HASHTAGS (
    id INT AUTO_INCREMENT PRIMARY KEY,
    paper_id INT NOT NULL,
    hashtag VARCHAR(50) NOT NULL,
    FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
    FOREIGN KEY (hashtag) REFERENCES HASHTAGS(hashtag)
);

CREATE TABLE PAPER_SUBMISSIONS (
    submission_id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    paper_id INT NOT NULL,
    accepted BOOLEAN DEFAULT FALSE,
    submitted_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES USERS(user_id),
    FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id)
);

CREATE TABLE PAPER_REVISIONS (
    revision_id INT AUTO_INCREMENT PRIMARY KEY,
    paper_id INT NOT NULL,
    user_id INT NOT NULL,
    title VARCHAR(255),
    description TEXT,
    format VARCHAR(100),
    revision_number INT DEFAULT 1,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    status VARCHAR(50) DEFAULT 'pending',
    FOREIGN KEY (paper_id) REFERENCES PAPERS(paper_id),
    FOREIGN KEY (user_id) REFERENCES USERS(user_id)
);

CREATE TABLE REVISION_GENRES (
    id INT AUTO_INCREMENT PRIMARY KEY,
    revision_id INT NOT NULL,
    genre VARCHAR(50) NOT NULL,
    FOREIGN KEY (revision_id) REFERENCES PAPER_REVISIONS(revision_id),
    FOREIGN KEY (genre) REFERENCES GENRES(genre)
);

CREATE TABLE REVISION_HASHTAGS (
    id INT AUTO_INCREMENT PRIMARY KEY,
    revision_id INT NOT NULL,
    hashtag VARCHAR(50) NOT NULL,
    FOREIGN KEY (revision_id) REFERENCES PAPER_REVISIONS(revision_id),
    FOREIGN KEY (hashtag) REFERENCES HASHTAGS(hashtag)
);

CREATE TABLE AUTHOR_SUBMISSIONS (
    author_submission_id INT AUTO_INCREMENT PRIMARY KEY,
    author_id INT NOT NULL,
    accepted BOOLEAN DEFAULT FALSE,
    submitted_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (author_id) REFERENCES AUTHORS(author_id)
);

CREATE TABLE AUTHOR_REVISIONS (
    author_revision_id INT AUTO_INCREMENT PRIMARY KEY,
    author_id INT NOT NULL,
    accepted BOOLEAN DEFAULT FALSE,
    submitted_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (author_id) REFERENCES AUTHORS(author_id)
);
DELIMITER $$
CREATE TRIGGER after_paper_read
AFTER INSERT ON PAPERS_USER
FOR EACH ROW
BEGIN
    -- Increase user's read_count
    UPDATE USERS
    SET read_count = read_count + 1
    WHERE user_id = NEW.user_id;

    -- Increase paper's popularity
    UPDATE PAPERS
    SET popularity = popularity + 1
    WHERE paper_id = NEW.paper_id;
    
    -- Recalculate average rating (assumes PAPERS_USER has a 'rating' column)
    UPDATE PAPERS p
    JOIN (
        SELECT paper_id, AVG(rating) AS new_average
        FROM PAPERS_USER
        WHERE paper_id = NEW.paper_id
    ) AS temp ON p.paper_id = temp.paper_id
    SET p.avg_score = temp.new_average
    WHERE p.paper_id = NEW.paper_id;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER after_user_contribution
AFTER INSERT ON POSTS
FOR EACH ROW
BEGIN
    UPDATE USERS
    SET contributions = contributions + 1
    WHERE user_id = NEW.user_id;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER after_revision_approve
AFTER UPDATE ON PAPER_REVISIONS
FOR EACH ROW
BEGIN
    IF NEW.status = 'approved' AND OLD.status <> 'approved' THEN
        UPDATE PAPERS
        SET title = NEW.title,
            description = NEW.description,
            format = NEW.format,
            updated_at = CURRENT_TIMESTAMP,
            accepted = TRUE
        WHERE paper_id = NEW.paper_id;
    END IF;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER update_timestamp
BEFORE UPDATE ON PAPERS
FOR EACH ROW
BEGIN
    SET NEW.updated_at = CURRENT_TIMESTAMP;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER update_paper_stats
AFTER INSERT ON PAPERS_USER
FOR EACH ROW
BEGIN
    UPDATE PAPERS
    SET popularity = popularity + 1
    WHERE paper_id = NEW.paper_id;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER after_papers_user_update
AFTER UPDATE ON PAPERS_USER
FOR EACH ROW
BEGIN
    UPDATE PAPERS p
    JOIN (
        SELECT paper_id, AVG(rating) AS new_average
        FROM PAPERS_USER
        WHERE paper_id = NEW.paper_id
    ) AS temp ON p.paper_id = temp.paper_id
    SET p.avg_score = temp.new_average
    WHERE p.paper_id = NEW.paper_id;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER before_user_insert
BEFORE INSERT ON USERS
FOR EACH ROW
BEGIN
    SET NEW.created_at = CURRENT_TIMESTAMP;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER after_papers_user_delete
AFTER DELETE ON PAPERS_USER
FOR EACH ROW
BEGIN
    UPDATE PAPERS
    SET popularity = popularity - 1
    WHERE paper_id = OLD.paper_id;

    UPDATE PAPERS p
    JOIN (
        SELECT paper_id, AVG(rating) AS new_average
        FROM PAPERS_USER
        WHERE paper_id = OLD.paper_id
    ) AS temp ON p.paper_id = temp.paper_id
    SET p.avg_score = IFNULL(temp.new_average, 0)
    WHERE p.paper_id = OLD.paper_id;
END$$
DELIMITER ;

DELIMITER $$
CREATE TRIGGER before_user_update
BEFORE UPDATE ON USERS
FOR EACH ROW
BEGIN
    IF NEW.last_login IS NULL THEN
        SET NEW.last_login = CURRENT_TIMESTAMP;
    END IF;
END$$
DELIMITER ;
