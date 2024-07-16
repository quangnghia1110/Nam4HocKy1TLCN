package studentConsulting.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.entity.authentication.UserEntity;

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
    private Long id;

    @Column(name = "question_id", length = 50, nullable = false)
    private String questionId;

    @Column(name = "consultant_id", length = 50, nullable = false)
    private Long consultantId;

    @Column(name = "department_id", length = 50, nullable = false)
    private String departmentId;

    @Column(name = "forwarded_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDateTime forwardedAt;

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    private ForwardStatus status = ForwardStatus.PENDING;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "question_id", insertable = false, updatable = false)
    private QuestionEntity question;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "consultant_id", insertable = false, updatable = false)
    private UserEntity consultant;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "department_id", insertable = false, updatable = false)
    private DepartmentEntity department;

    public enum ForwardStatus {
        PENDING, SEEN_BY_DEPARTMENT, RESOLVED
    }
}
