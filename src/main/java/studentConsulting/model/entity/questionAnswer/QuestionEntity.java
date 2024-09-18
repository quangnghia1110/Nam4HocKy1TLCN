package studentConsulting.model.entity.questionAnswer;

import java.time.LocalDate;
import java.time.LocalDate;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;

@Data
@Builder
@Entity
@Table(name = "questions")
@NoArgsConstructor
@AllArgsConstructor
public class QuestionEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column( name = "id")
    private Integer id;

    @Column(name = "created_at",  updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "user_id",  referencedColumnName = "id")
    private UserInformationEntity user; 

    @ManyToOne
    @JoinColumn(name = "department_id",  referencedColumnName = "id")
    private DepartmentEntity department;

    @ManyToOne
    @JoinColumn(name = "field_id",  referencedColumnName = "id")
    private FieldEntity field; 

    @ManyToOne
    @JoinColumn(name = "role_ask_id",  referencedColumnName = "id")
    private RoleAskEntity roleAsk; 

    @ManyToOne
    @JoinColumn(name = "parent_question_id", referencedColumnName = "id")
    private QuestionEntity parentQuestion; 

    @Column(name = "title",  length = 255)
    private String title; 

    @Column(name = "content",  length = 900)
    private String content; 

    @Column(name = "views")
    private Integer views; 

    @Column(name = "file_name", length = 255)
    private String fileName; 

    @Column(name = "status_approval")
    private Boolean statusApproval; 

    @Column(name = "status_public")
    private Boolean statusPublic; 

    @Column(name = "status_delete")
    private Boolean statusDelete;

    @OneToMany(mappedBy = "parentQuestion", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;

    @OneToMany(mappedBy = "question", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardQuestionEntity> forwardQuestions;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "question", cascade = CascadeType.ALL)
    private Set<AnswerEntity> answers;

    @OneToMany(mappedBy = "question", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private Set<DeletionLogEntity> deletionLog;
    
    
    
}
