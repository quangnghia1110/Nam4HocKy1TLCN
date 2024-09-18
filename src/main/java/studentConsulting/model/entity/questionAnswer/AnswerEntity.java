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
import java.time.LocalDate;

@Data
@Builder
@Entity
@Table(name = "answers")
@NoArgsConstructor
@AllArgsConstructor
public class AnswerEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column( name = "id")
    private Integer id;

    @Column(name = "created_at",  updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "question_id",  referencedColumnName = "id")
    @JsonIgnore
    private QuestionEntity question; 

    @ManyToOne
    @JoinColumn(name = "role_consultant_id",  referencedColumnName = "id")
    private RoleConsultantEntity roleConsultant; 

    @ManyToOne
    @JoinColumn(name = "user_id",  referencedColumnName = "id")
    private UserInformationEntity user; 

    @Column(name = "title",  length = 255)
    private String title; 

    @Column(name = "content",  length = 255)
    private String content; 

    @Column(name = "file",  length = 255)
    private String file; 

    @Column(name = "status_approval")
    private Boolean statusApproval; 

    @Column(name = "status_answer")
    private Boolean statusAnswer; 
}
