package studentConsulting.model.entity.question_answer;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.user.RoleConsultantEntity;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.*;
import java.time.LocalDate;

@Data
@Builder
@Entity
@Table(name = "answer")
@NoArgsConstructor
@AllArgsConstructor
public class AnswerEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "question_id", referencedColumnName = "id")
    @JsonIgnore
    private QuestionEntity question;

    @ManyToOne
    @JoinColumn(name = "role_consultant_id", referencedColumnName = "id")
    private RoleConsultantEntity roleConsultant;

    @ManyToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserInformationEntity user;

    @Column(name = "title", length = 255)
    private String title;

    @Column(name = "content", length = 255)
    private String content;

    @Column(name = "file", length = 255)
    private String file;

    @Column(name = "status_approval")
    private Boolean statusApproval;

    @Column(name = "status_answer")
    private Boolean statusAnswer;
}
