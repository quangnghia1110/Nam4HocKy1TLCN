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
@Table(name = "questions")
@NoArgsConstructor
@AllArgsConstructor
public class QuestionEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã câu hỏi

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; // Mã phòng ban tham chiếu

    @ManyToOne
    @JoinColumn(name = "field_id", nullable = false, referencedColumnName = "id")
    private FieldEntity field; // Mã lĩnh vực tham chiếu

    @ManyToOne
    @JoinColumn(name = "role_ask_id", nullable = false, referencedColumnName = "id")
    private RoleAskEntity roleAsk; // Mã vai trò hỏi tham chiếu

    @ManyToOne
    @JoinColumn(name = "parent_question_id", referencedColumnName = "id")
    private QuestionEntity parentQuestion; // Mã câu hỏi cha tham chiếu

    @Column(name = "title", nullable = false, length = 255)
    private String title; // Tiêu đề câu hỏi

    @Column(name = "content", nullable = false, length = 900)
    private String content; // Nội dung câu hỏi

    @Column(name = "views")
    private Integer views; // Lượt xem

    @Column(name = "file_name", length = 255)
    private String fileName; // Tên file đính kèm

    @Column(name = "status_approval", nullable = false)
    private Boolean statusApproval; // Được chấp nhận, không được chấp nhận

    @Column(name = "status_public", nullable = false)
    private Boolean statusPublic; // Công khai, riêng tư

    @Column(name = "status_delete", nullable = false)
    private Boolean statusDelete; // Đã xóa, chưa xóa

    @Column(name = "created_at", nullable = false)
    private Timestamp createdAt; // Thời gian tạo

    @Column(name = "updated_at", nullable = false, insertable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật
    
    @OneToMany(mappedBy = "parentQuestion", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;
    
    @OneToMany(mappedBy = "question", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AnswerEntity> answers;
}
