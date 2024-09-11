package studentConsulting.model.entity.questionAnswer;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;

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
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private LocalDateTime updatedAt;

    @ManyToOne
    @JoinColumn(name = "question_id", nullable = false, referencedColumnName = "id")
    @JsonIgnore
    private QuestionEntity question; 

    @ManyToOne
    @JoinColumn(name = "role_consultant_id", nullable = false, referencedColumnName = "id")
    private RoleConsultantEntity roleConsultant; 

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; 

    @Column(name = "title", nullable = false, length = 255)
    private String title; 

    @Column(name = "content", nullable = false, length = 255)
    private String content; 

    @Column(name = "file", nullable = false, length = 255)
    private String file; 

    @Column(name = "status_approval", nullable = false)
    private Boolean statusApproval; 

    @Column(name = "status_answer", nullable = false)
    private Boolean statusAnswer; 
}
