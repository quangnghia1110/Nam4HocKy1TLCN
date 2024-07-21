package studentConsulting.model.entity.departmentField;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.ForwardQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import java.sql.Timestamp;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "departments")
@NoArgsConstructor
@AllArgsConstructor
public class DepartmentEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã phòng ban

    @Column(name = "name", nullable = false, length = 255, unique = true)
    private String name; // Tên phòng ban

    @Column(name = "description", length = 500)
    private String description; // Mô tả phòng ban

    @Column(name = "logo", length = 255)
    private String logo; // Logo phòng ban

    @Column(name = "created_at", nullable = false, updatable = false)
    private Timestamp createdAt; // Thời gian tạo

    @Column(name = "updated_at", nullable = false)
    private Timestamp updatedAt; // Thời gian cập nhật
    
    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<FieldEntity> fields; // Ensure this is the correct relationship
    
    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NotificationEntity> notifications;
    
    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;
    
    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<CommonQuestionEntity> commonQuestions;
    
    @OneToMany(mappedBy = "fromDepartment", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardQuestionEntity> fromForwardQuestions;
    
    @OneToMany(mappedBy = "toDepartment", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardQuestionEntity> toForwardQuestions;
}
