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
import java.time.LocalDate;
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
    private LocalDate createdAt;

    @Column(name = "updated_at", nullable = true, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private LocalDate updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity user;

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; 

    @ManyToOne
    @JoinColumn(name = "field_id", nullable = false, referencedColumnName = "id")
    private FieldEntity field; 

    @ManyToOne
    @JoinColumn(name = "role_ask_id", nullable = false, referencedColumnName = "id")
    private RoleAskEntity roleAsk; 
    
    @Column(name = "title", nullable = false, length = 255)
    private String title; 

    @Column(name = "content", nullable = false, length = 900)
    private String content; 

    @Column(name = "views", nullable = true)
    private Integer views; 

    @Column(name = "file_name", length = 255)
    private String fileName; 
    
    @Column(name = "status")
    private Integer status;
    
    @Column(name = "answer_title", length = 900)
    private String answerTitle;
    
    @Column(name = "answer_content", length = 900)
    private String answerContent; 

    @Column(name = "answer_user_email", length = 255)
    private String answerUserEmail; 
    
    @Column(name = "answer_created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDate answerCreatedAt; 
    
    @Column(name = "answer_user_firstname", length = 255)
    private String answerUserFirstname; 
    
    @Column(name = "answer_user_lastname", length = 255)
    private String answerUserLastname;

    @Column(name = "asker_firstname", length = 255)
    private String askerFirstname;

    @Column(name = "asker_lastname", length = 255)
    private String askerLastname;
}
