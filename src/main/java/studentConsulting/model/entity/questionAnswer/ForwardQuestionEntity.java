package studentConsulting.model.entity.questionAnswer;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;

import java.sql.Timestamp;
import java.time.LocalDateTime;

@Data
@Builder
@Entity
@Table(name = "forward_questions")
@NoArgsConstructor
@AllArgsConstructor
public class ForwardQuestionEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã câu hỏi tham chiếu

    @ManyToOne
    @JoinColumn(name = "from_department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity fromDepartment; // Từ phòng ban tham chiếu

    @ManyToOne
    @JoinColumn(name = "to_department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity toDepartment; // Đến phòng ban tham chiếu

    @ManyToOne
    @JoinColumn(name = "question_id", nullable = false, referencedColumnName = "id")
    private QuestionEntity question; // Mã câu hỏi tham chiếu

    @Column(name = "title", nullable = false, length = 255)
    private String title; // Tiêu đề

    @Column(name = "content", nullable = false, length = 255)
    private String content; // Nội dung

    @Column(name = "status_forward", nullable = false)
    private Boolean statusForward; // Đã chuyển tiếp, chưa chuyển tiếp

    @Column(name = "created_at", nullable = false)
    private Timestamp createdAt; // Thời gian chuyển tiếp câu hỏi

    @Column(name = "updated_at", nullable = false, insertable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật
}
