use tlcn;

-- Tạo bảng roles
CREATE TABLE IF NOT EXISTS roles (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255)
) ENGINE=InnoDB;

-- Tạo bảng departments
CREATE TABLE IF NOT EXISTS departments (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    description VARCHAR(500),
    logo VARCHAR(255),
    name VARCHAR(255)
) ENGINE=InnoDB;

-- Tạo bảng provinces
CREATE TABLE IF NOT EXISTS provinces (
    code VARCHAR(20) PRIMARY KEY,
    code_name VARCHAR(255),
    full_name VARCHAR(255),
    full_name_en VARCHAR(255),
    name VARCHAR(255),
    name_en VARCHAR(255)
) ENGINE=InnoDB;

-- Tạo bảng districts
CREATE TABLE IF NOT EXISTS districts (
    code VARCHAR(20) PRIMARY KEY,
    code_name VARCHAR(255),
    full_name VARCHAR(255),
    full_name_en VARCHAR(255),
    name VARCHAR(255),
    name_en VARCHAR(255),
    province_code VARCHAR(20)
) ENGINE=InnoDB;

-- Tạo bảng wards
CREATE TABLE IF NOT EXISTS wards (
    code VARCHAR(20) PRIMARY KEY,
    code_name VARCHAR(255),
    full_name VARCHAR(255),
    full_name_en VARCHAR(255),
    name VARCHAR(255),
    name_en VARCHAR(255),
    district_code VARCHAR(20)
) ENGINE=InnoDB;

-- Tạo bảng role_consultant
CREATE TABLE IF NOT EXISTS role_consultant (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    name VARCHAR(50),
    role_id INT
) ENGINE=InnoDB;

-- Tạo bảng role_ask
CREATE TABLE IF NOT EXISTS role_ask (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    name VARCHAR(50),
    role_id INT
) ENGINE=InnoDB;

-- Tạo bảng account
CREATE TABLE IF NOT EXISTS account (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    email VARCHAR(50),
    is_activity BIT,
    password VARCHAR(255),
    username VARCHAR(50),
    verify_code VARCHAR(50),
    verify_code_attempt_count INT DEFAULT 0,
    verify_code_expiration_time DATETIME(6),
    verify_register VARCHAR(50),
    department_id INT,
    role_id INT,
    role_consultant_id INT
) ENGINE=InnoDB;

-- Tạo bảng address
CREATE TABLE IF NOT EXISTS address (
    id INT AUTO_INCREMENT PRIMARY KEY,
    line VARCHAR(255),
    districts_id VARCHAR(20),
    provinces_id VARCHAR(20),
    wards_id VARCHAR(20)
) ENGINE=InnoDB;

-- Tạo bảng user_information
CREATE TABLE IF NOT EXISTS user_information (
    id INT AUTO_INCREMENT PRIMARY KEY,
    avatar_url VARCHAR(900),
    created_at DATE,
    firstname VARCHAR(50),
    gender VARCHAR(3),
    lastname VARCHAR(50),
    phone VARCHAR(10),
    school_name VARCHAR(255),
    student_code VARCHAR(50),
    account_id INT,
    address_id INT,
    conversation_id INT
) ENGINE=InnoDB;

-- Tạo bảng fields
CREATE TABLE IF NOT EXISTS fields (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    name VARCHAR(255),
    department_id INT
) ENGINE=InnoDB;

-- Tạo bảng questions
CREATE TABLE IF NOT EXISTS questions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(900),
    created_at DATE,
    file_name VARCHAR(255),
    status_approval BIT,
    status_delete BIT,
    status_public BIT,
    title VARCHAR(255),
    views INT,
    department_id INT,
    field_id INT,
    parent_question_id INT,
    role_ask_id INT,
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng answers
CREATE TABLE IF NOT EXISTS answers (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(255),
    created_at DATE,
    file VARCHAR(255),
    status_answer BIT,
    status_approval BIT,
    title VARCHAR(255),
    question_id INT,
    role_consultant_id INT,
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng posts
CREATE TABLE IF NOT EXISTS posts (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(255),
    created_at DATE,
    file_name VARCHAR(255),
    is_anonymous BIT NOT NULL,
    is_approved BIT NOT NULL,
    views INT NOT NULL,
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng comment
CREATE TABLE IF NOT EXISTS comment (
    id_comment INT AUTO_INCREMENT PRIMARY KEY,
    comment VARCHAR(255) NOT NULL,
    create_date DATE NOT NULL,
    id_comment_father INT,
    id_post INT,
    id_user_comment INT
) ENGINE=InnoDB;

-- Tạo bảng consultation_schedule
CREATE TABLE IF NOT EXISTS consultation_schedule (
    id INT AUTO_INCREMENT PRIMARY KEY,
    consultation_date DATE,
    consultation_time VARCHAR(255),
    content VARCHAR(255),
    created_at DATE,
    link VARCHAR(255),
    location VARCHAR(255),
    mode BIT,
    status_confirmed BIT,
    status_public BIT,
    title VARCHAR(255),
    consultant_id INT,
    department_id INT,
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng conversations
CREATE TABLE IF NOT EXISTS conversations (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    is_group BIT,
    name VARCHAR(255),
    status_active BIT,
    consultant_id INT,
    department_id INT,
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng conversation_user
CREATE TABLE IF NOT EXISTS conversation_user (
    conversation_id INT NOT NULL,
    user_id INT NOT NULL,
    PRIMARY KEY (conversation_id, user_id)
) ENGINE=InnoDB;

-- Tạo bảng deletion_log
CREATE TABLE IF NOT EXISTS deletion_log (
    id INT AUTO_INCREMENT PRIMARY KEY,
    deleted_at DATE NOT NULL,
    deleted_by VARCHAR(255) NOT NULL,
    reason VARCHAR(255) NOT NULL,
    question_id INT
) ENGINE=InnoDB;

-- Tạo bảng forward_questions
CREATE TABLE IF NOT EXISTS forward_questions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    status_forward BIT,
    title VARCHAR(255),
    from_department_id INT,
    question_id INT,
    to_department_id INT
) ENGINE=InnoDB;

-- Tạo bảng like_record
CREATE TABLE IF NOT EXISTS like_record (
    id_target INT NOT NULL,
    type VARCHAR(255) NOT NULL,
    id_user INT NOT NULL,
    PRIMARY KEY (id_target, type, id_user)
) ENGINE=InnoDB;

-- Tạo bảng messages
CREATE TABLE IF NOT EXISTS messages (
    id INT AUTO_INCREMENT PRIMARY KEY,
    conversation_id INT NOT NULL,
    sender_id INT NOT NULL,
    receiver_id INT NOT NULL,
    message VARCHAR(255) NOT NULL,
    date DATE NOT NULL,
    message_status VARCHAR(50) NOT NULL,
    FOREIGN KEY (conversation_id) REFERENCES conversations(id),
    FOREIGN KEY (sender_id) REFERENCES user_information(id),
    FOREIGN KEY (receiver_id) REFERENCES user_information(id)
) ENGINE=InnoDB;

-- Tạo bảng notification
CREATE TABLE IF NOT EXISTS notification (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(255),
    notification_type VARCHAR(255),
    receiver_id INT,
    sender_id INT,
    status VARCHAR(255),
    time DATETIME(6)
) ENGINE=InnoDB;

-- Tạo bảng ratings
CREATE TABLE IF NOT EXISTS ratings (
    id INT AUTO_INCREMENT PRIMARY KEY,
    attitude INT,
    attitude_comment TEXT,
    expertise_comment TEXT,
    expertise_knowledge INT,
    general_comment TEXT,
    general_satisfaction INT,
    response_speed INT,
    response_speed_comment TEXT,
    submitted_at DATE,
    understanding INT,
    understanding_comment TEXT,
    consultant_id INT,
    department_id INT,
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng role_auth
CREATE TABLE IF NOT EXISTS role_auth (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    expired_time BIGINT,
    token_id VARCHAR(255),
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng user_fields
CREATE TABLE IF NOT EXISTS user_fields (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE,
    field_id INT,
    user_id INT
) ENGINE=InnoDB;

-- Tạo bảng common_questions
CREATE TABLE IF NOT EXISTS common_questions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    answer_content VARCHAR(900),
    answer_created_at DATE,
    answer_title VARCHAR(900),
    answer_user_email VARCHAR(255),
    answer_user_firstname VARCHAR(255),
    answer_user_lastname VARCHAR(255),
    asker_firstname VARCHAR(255),
    asker_lastname VARCHAR(255),
    content VARCHAR(900),
    created_at DATE,
    file_name VARCHAR(255),
    status INT,
    title VARCHAR(255),
    views INT,
    department_id INT,
    field_id INT,
    role_ask_id INT,
    user_id INT
) ENGINE=InnoDB;

-- Thêm khóa ngoại và constraint
ALTER TABLE districts ADD FOREIGN KEY (province_code) REFERENCES provinces(code);
ALTER TABLE wards ADD FOREIGN KEY (district_code) REFERENCES districts(code);
ALTER TABLE role_consultant ADD FOREIGN KEY (role_id) REFERENCES roles(id);
ALTER TABLE role_ask ADD FOREIGN KEY (role_id) REFERENCES roles(id);
ALTER TABLE account ADD FOREIGN KEY (department_id) REFERENCES departments(id);
ALTER TABLE account ADD FOREIGN KEY (role_id) REFERENCES roles(id);
ALTER TABLE account ADD FOREIGN KEY (role_consultant_id) REFERENCES role_consultant(id);
ALTER TABLE address ADD FOREIGN KEY (districts_id) REFERENCES districts(code);
ALTER TABLE address ADD FOREIGN KEY (provinces_id) REFERENCES provinces(code);
ALTER TABLE address ADD FOREIGN KEY (wards_id) REFERENCES wards(code);
ALTER TABLE user_information ADD FOREIGN KEY (account_id) REFERENCES account(id);
ALTER TABLE user_information ADD FOREIGN KEY (address_id) REFERENCES address(id);
ALTER TABLE user_information ADD FOREIGN KEY (conversation_id) REFERENCES conversations(id);
ALTER TABLE fields ADD FOREIGN KEY (department_id) REFERENCES departments(id);
ALTER TABLE questions ADD FOREIGN KEY (department_id) REFERENCES departments(id);
ALTER TABLE questions ADD FOREIGN KEY (field_id) REFERENCES fields(id);
ALTER TABLE questions ADD FOREIGN KEY (parent_question_id) REFERENCES questions(id);
ALTER TABLE questions ADD FOREIGN KEY (role_ask_id) REFERENCES role_ask(id);
ALTER TABLE questions ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE answers ADD FOREIGN KEY (question_id) REFERENCES questions(id);
ALTER TABLE answers ADD FOREIGN KEY (role_consultant_id) REFERENCES role_consultant(id);
ALTER TABLE answers ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE posts ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE comment ADD FOREIGN KEY (id_comment_father) REFERENCES comment(id_comment);
ALTER TABLE comment ADD FOREIGN KEY (id_post) REFERENCES posts(id);
ALTER TABLE comment ADD FOREIGN KEY (id_user_comment) REFERENCES user_information(id);
ALTER TABLE consultation_schedule ADD FOREIGN KEY (consultant_id) REFERENCES user_information(id);
ALTER TABLE consultation_schedule ADD FOREIGN KEY (department_id) REFERENCES departments(id);
ALTER TABLE consultation_schedule ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE conversations ADD FOREIGN KEY (consultant_id) REFERENCES user_information(id);
ALTER TABLE conversations ADD FOREIGN KEY (department_id) REFERENCES departments(id);
ALTER TABLE conversations ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE conversation_user ADD FOREIGN KEY (conversation_id) REFERENCES conversations(id);
ALTER TABLE conversation_user ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE deletion_log ADD FOREIGN KEY (question_id) REFERENCES questions(id);
ALTER TABLE forward_questions ADD FOREIGN KEY (from_department_id) REFERENCES departments(id);
ALTER TABLE forward_questions ADD FOREIGN KEY (question_id) REFERENCES questions(id);
ALTER TABLE forward_questions ADD FOREIGN KEY (to_department_id) REFERENCES departments(id);
ALTER TABLE like_record ADD FOREIGN KEY (id_user) REFERENCES user_information(id);
ALTER TABLE messages ADD FOREIGN KEY (conversation_id) REFERENCES conversations(id);
ALTER TABLE notification ADD FOREIGN KEY (receiver_id) REFERENCES user_information(id);
ALTER TABLE notification ADD FOREIGN KEY (sender_id) REFERENCES user_information(id);
ALTER TABLE ratings ADD FOREIGN KEY (consultant_id) REFERENCES user_information(id);
ALTER TABLE ratings ADD FOREIGN KEY (department_id) REFERENCES departments(id);
ALTER TABLE ratings ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE role_auth ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE user_fields ADD FOREIGN KEY (field_id) REFERENCES fields(id);
ALTER TABLE user_fields ADD FOREIGN KEY (user_id) REFERENCES user_information(id);
ALTER TABLE common_questions ADD FOREIGN KEY (department_id) REFERENCES departments(id);
ALTER TABLE common_questions ADD FOREIGN KEY (field_id) REFERENCES fields(id);
ALTER TABLE common_questions ADD FOREIGN KEY (role_ask_id) REFERENCES role_ask(id);
ALTER TABLE common_questions ADD FOREIGN KEY (user_id) REFERENCES user_information(id);

