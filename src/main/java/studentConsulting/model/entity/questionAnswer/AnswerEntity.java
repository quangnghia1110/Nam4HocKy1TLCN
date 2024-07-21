package studentConsulting.model.entity.questionAnswer;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;

import java.sql.Timestamp;
import java.time.LocalDateTime;

@Data
@Builder
@Entity
@Table(name = "answers")
@NoArgsConstructor
@AllArgsConstructor
public class AnswerEntity {

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã câu trả lời

    @ManyToOne
    @JoinColumn(name = "question_id", nullable = false, referencedColumnName = "id")
    private QuestionEntity question; // Mã câu hỏi tham chiếu

    @ManyToOne
    @JoinColumn(name = "role_consultant_id", nullable = false, referencedColumnName = "id")
    private RoleConsultantEntity roleConsultant; // Mã vai trò tư vấn tham chiếu

    @Column(name = "title", nullable = false, length = 255)
    private String title; // Tiêu đề

    @Column(name = "content", nullable = false, length = 255)
    private String content; // Nội dung

    @Column(name = "file", nullable = false, length = 255)
    private String file; // File đính kèm png, jpg, jpeg

    @Column(name = "status_approval", nullable = false)
    private Boolean statusApproval; // Cần kiểm duyệt, không cần kiểm duyệt

    @Column(name = "status_answer", nullable = false)
    private Boolean statusAnswer; // Đã trả lời, chưa trả lời

    @Column(name = "created_at", nullable = false)
    private Timestamp createdAt; // Thời gian tạo câu trả lời

    @Column(name = "updated_at", nullable = false, insertable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật
}
