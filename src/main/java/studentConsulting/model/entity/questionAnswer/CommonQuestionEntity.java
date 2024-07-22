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

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người soạn tham chiếu

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; // Mã phòng ban tham chiếu

    @ManyToOne
    @JoinColumn(name = "field_id", nullable = false, referencedColumnName = "id")
    private FieldEntity field; // Mã lĩnh vực tham chiếu

    @Column(name = "title", nullable = false, length = 255)
    private String title; // Tiêu đề

    @Column(name = "content_question", nullable = false, length = 255)
    private String contentQuestion; // Nội dung câu hỏi

    @Column(name = "content_answer", nullable = false, length = 255)
    private String contentAnswer; // Nội dung câu trả lời

    @Column(name = "views")
    private Integer views; // Tổng số view

    @Column(name = "status")
    private Integer status; // Đã đăng, chưa đăng
}
