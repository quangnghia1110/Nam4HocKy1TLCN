use tlcn;

-- Tạo bảng roles
CREATE TABLE IF NOT EXISTS roles (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NULL
) ENGINE=InnoDB;

-- Tạo bảng departments
CREATE TABLE IF NOT EXISTS departments (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    description VARCHAR(500) NULL,
    logo VARCHAR(255) NULL,
    name VARCHAR(255) NULL
) ENGINE=InnoDB;

-- Tạo bảng provinces
CREATE TABLE IF NOT EXISTS provinces (
    code VARCHAR(20) PRIMARY KEY,
    code_name VARCHAR(255) NULL,
    full_name VARCHAR(255) NULL,
    full_name_en VARCHAR(255) NULL,
    name VARCHAR(255) NULL,
    name_en VARCHAR(255) NULL
) ENGINE=InnoDB;

-- Tạo bảng districts
CREATE TABLE IF NOT EXISTS districts (
    code VARCHAR(20) PRIMARY KEY,
    code_name VARCHAR(255) NULL,
    full_name VARCHAR(255) NULL,
    full_name_en VARCHAR(255) NULL,
    name VARCHAR(255) NULL,
    name_en VARCHAR(255) NULL,
    province_code VARCHAR(20) NULL
) ENGINE=InnoDB;

-- Tạo bảng wards
CREATE TABLE IF NOT EXISTS wards (
    code VARCHAR(20) PRIMARY KEY,
    code_name VARCHAR(255) NULL,
    full_name VARCHAR(255) NULL,
    full_name_en VARCHAR(255) NULL,
    name VARCHAR(255) NULL,
    name_en VARCHAR(255) NULL,
    district_code VARCHAR(20) NULL
) ENGINE=InnoDB;

-- Tạo bảng role_consultant
CREATE TABLE IF NOT EXISTS role_consultant (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    name VARCHAR(50) NULL,
    role_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng role_ask
CREATE TABLE IF NOT EXISTS role_ask (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    name VARCHAR(50) NULL,
    role_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng account
CREATE TABLE IF NOT EXISTS account (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    email VARCHAR(50) NULL,
    is_activity BIT NULL,
    password VARCHAR(255) NULL,
    username VARCHAR(50) NULL,
    verify_code VARCHAR(50) NULL,
    verify_code_attempt_count INT DEFAULT 0 NULL,
    verify_code_expiration_time DATETIME(6) NULL,
    verify_register VARCHAR(50) NULL,
    department_id INT NULL,
    role_id INT NULL,
    role_consultant_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng address
CREATE TABLE IF NOT EXISTS address (
    id INT AUTO_INCREMENT PRIMARY KEY,
    line VARCHAR(255) NULL,
    districts_id VARCHAR(20) NULL,
    provinces_id VARCHAR(20) NULL,
    wards_id VARCHAR(20) NULL
) ENGINE=InnoDB;

-- Tạo bảng user_information
CREATE TABLE IF NOT EXISTS user_information (
    id INT AUTO_INCREMENT PRIMARY KEY,
    avatar_url VARCHAR(900) NULL,
    created_at DATE NULL,
    firstname VARCHAR(50) NULL,
    gender VARCHAR(3) NULL,
    lastname VARCHAR(50) NULL,
    phone VARCHAR(10) NULL,
    school_name VARCHAR(255) NULL,
    student_code VARCHAR(50) NULL,
    account_id INT NULL,
    address_id INT NULL,
    conversation_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng fields
CREATE TABLE IF NOT EXISTS fields (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    name VARCHAR(255) NULL,
    department_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng questions
CREATE TABLE IF NOT EXISTS questions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(900) NULL,
    created_at DATE NULL,
    file_name VARCHAR(255) NULL,
    status_approval BIT NULL,
    status_delete BIT NULL,
    status_public BIT NULL,
    title VARCHAR(255) NULL,
    views INT NULL,
    department_id INT NULL,
    field_id INT NULL,
    parent_question_id INT NULL,
    role_ask_id INT NULL,
    user_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng answers
CREATE TABLE IF NOT EXISTS answers (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(255) NULL,
    created_at DATE NULL,
    file VARCHAR(255) NULL,
    status_answer BIT NULL,
    status_approval BIT NULL,
    title VARCHAR(255) NULL,
    question_id INT NULL,
    role_consultant_id INT NULL,
    user_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng posts
CREATE TABLE IF NOT EXISTS posts (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(255) NULL,
    created_at DATE NULL,
    file_name VARCHAR(255) NULL,
    is_anonymous BIT NOT NULL,
    is_approved BIT NOT NULL,
    views INT NOT NULL,
    user_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng comment
CREATE TABLE IF NOT EXISTS comment (
    id_comment INT AUTO_INCREMENT PRIMARY KEY,
    comment VARCHAR(255) NULL,
    create_date DATE NULL,
    id_comment_father INT NULL,
    id_post INT NULL,
    id_user_comment INT NULL
) ENGINE=InnoDB;

-- Tạo bảng consultation_schedule
CREATE TABLE IF NOT EXISTS consultation_schedule (
    id INT AUTO_INCREMENT PRIMARY KEY,
    consultation_date DATE NULL,
    consultation_time VARCHAR(255) NULL,
    content VARCHAR(255) NULL,
    created_at DATE NULL,
    link VARCHAR(255) NULL,
    location VARCHAR(255) NULL,
    mode BIT NULL,
    status_confirmed BIT NULL,
    status_public BIT NULL,
    title VARCHAR(255) NULL,
    consultant_id INT NULL,
    department_id INT NULL,
    user_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng conversations
CREATE TABLE IF NOT EXISTS conversations (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    is_group BIT NULL,
    name VARCHAR(255) NULL,
    status_active BIT NULL,
    consultant_id INT NULL,
    department_id INT NULL,
    user_id INT NULL
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
    deleted_at DATE NULL,
    deleted_by VARCHAR(255) NULL,
    reason VARCHAR(255) NULL,
    question_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng forward_questions
CREATE TABLE IF NOT EXISTS forward_questions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    status_forward BIT NULL,
    title VARCHAR(255) NULL,
    from_department_id INT NULL,
    question_id INT NULL,
    to_department_id INT NULL
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
    conversation_id INT NULL,
    sender_id INT NULL,
    receiver_id INT NULL,
    message VARCHAR(255) NULL,
    image_url VARCHAR(255) NULL,
    date DATE NULL,
    message_status VARCHAR(50) NULL,
    FOREIGN KEY (conversation_id) REFERENCES conversations(id),
    FOREIGN KEY (sender_id) REFERENCES user_information(id),
    FOREIGN KEY (receiver_id) REFERENCES user_information(id)
) ENGINE=InnoDB;

-- Tạo bảng notification
CREATE TABLE IF NOT EXISTS notification (
    id INT AUTO_INCREMENT PRIMARY KEY,
    content VARCHAR(255) NULL,
    notification_type VARCHAR(255) NULL,
    receiver_id INT NULL,
    sender_id INT NULL,
    status VARCHAR(255) NULL,
    time DATETIME(6) NULL
) ENGINE=InnoDB;

-- Tạo bảng ratings
CREATE TABLE IF NOT EXISTS ratings (
    id INT AUTO_INCREMENT PRIMARY KEY,
    attitude INT NULL,
    attitude_comment TEXT NULL,
    expertise_comment TEXT NULL,
    expertise_knowledge INT NULL,
    general_comment TEXT NULL,
    general_satisfaction INT NULL,
    response_speed INT NULL,
    response_speed_comment TEXT NULL,
    submitted_at DATE NULL,
    understanding INT NULL,
    understanding_comment TEXT NULL,
    consultant_id INT NULL,
    department_id INT NULL,
    user_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng role_auth
CREATE TABLE IF NOT EXISTS role_auth (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    expired_time BIGINT NULL,
    token_id VARCHAR(255) NULL,
    user_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng user_fields
CREATE TABLE IF NOT EXISTS user_fields (
    id INT AUTO_INCREMENT PRIMARY KEY,
    created_at DATE NULL,
    field_id INT NULL,
    user_id INT NULL
) ENGINE=InnoDB;

-- Tạo bảng common_questions
CREATE TABLE IF NOT EXISTS common_questions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    answer_content VARCHAR(900) NULL,
    answer_created_at DATE NULL,
    answer_title VARCHAR(900) NULL,
    answer_user_email VARCHAR(255) NULL,
    answer_user_firstname VARCHAR(255) NULL,
    answer_user_lastname VARCHAR(255) NULL,
    asker_firstname VARCHAR(255) NULL,
    asker_lastname VARCHAR(255) NULL,
    content VARCHAR(900) NULL,
    created_at DATE NULL,
    file_name VARCHAR(255) NULL,
    status INT NULL,
    title VARCHAR(255) NULL,
    views INT NULL,
    department_id INT NULL,
    field_id INT NULL,
    role_ask_id INT NULL,
    user_id INT NULL
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS consultation_schedule_registration (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    consultation_schedule_id INT NOT NULL,
    registered_at DATETIME NULL,
    status BOOLEAN NULL,
    FOREIGN KEY (user_id) REFERENCES user_information(id),
    FOREIGN KEY (consultation_schedule_id) REFERENCES consultation_schedule(id)
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

