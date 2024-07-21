package studentConsulting.model.entity.notification;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import javax.persistence.*;
import java.sql.Timestamp;

@Data
@Builder
@Entity
@Table(name = "notifications")
@NoArgsConstructor
@AllArgsConstructor
public class NotificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã thông báo

    @ManyToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; // Mã phòng ban tham chiếu

    @ManyToOne
    @JoinColumn(name = "question_id", nullable = false, referencedColumnName = "id")
    private QuestionEntity question; // Mã câu hỏi tham chiếu

    @Column(name = "title", nullable = false, length = 255)
    private String title; // Tiêu đề

    @Column(name = "content", nullable = false, length = 255)
    private String content; // Nội dung

    @Column(name = "type", nullable = false, length = 255)
    private String type; // 'consultation_schedule', 'answer_approval', 'question_approval', 'news_approval', 'user_rating', 'consultant_rating'

    @Column(name = "status_read", nullable = false)
    private Boolean statusRead; // Đã đọc, chưa đọc

    @Column(name = "created_at")
    private Timestamp createdAt; // Thời gian tạo thông báo

    @Column(name = "updated_at", insertable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật
}
