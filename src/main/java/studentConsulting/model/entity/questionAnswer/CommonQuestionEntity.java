package studentConsulting.model.entity.questionAnswer;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "common_questions")
@NoArgsConstructor
@AllArgsConstructor
public class CommonQuestionEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = true, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = true, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private LocalDateTime updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người soạn tham chiếu

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; // Mã phòng ban tham chiếu

    @ManyToOne
    @JoinColumn(name = "field_id", nullable = false, referencedColumnName = "id")
    private FieldEntity field; // Mã lĩnh vực tham chiếu

    @ManyToOne
    @JoinColumn(name = "role_ask_id", nullable = false, referencedColumnName = "id")
    private RoleAskEntity roleAsk; // Mã vai trò hỏi tham chiếu
    
    @Column(name = "title", nullable = false, length = 255)
    private String title; // Tiêu đề

    @Column(name = "content", nullable = false, length = 900)
    private String content; // Nội dung câu hỏi

    @Column(name = "views", nullable = true)
    private Integer views; // Lượt xem

    @Column(name = "file_name", length = 255)
    private String fileName; // Tên file đính kèm
    
    @Column(name = "status")
    private Integer status; // Đã đăng, chưa đăng
    
    @Column(name = "answer_title", length = 900)
    private String answerTitle;
    
    @Column(name = "answer_content", length = 900)
    private String answerContent; 

    @Column(name = "answer_user_email", length = 255)
    private String answerUserEmail; 
    
    @Column(name = "answer_created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDateTime answerCreatedAt; 
    
    @Column(name = "answer_user_firstname", length = 255)
    private String answerUserFirstname; 
    
    @Column(name = "answer_user_lastname", length = 255)
    private String answerUserLastname;

    @Column(name = "asker_firstname", length = 255)
    private String askerFirstname;

    @Column(name = "asker_lastname", length = 255)
    private String askerLastname;
}
