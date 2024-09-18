package studentConsulting.model.entity.communication;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDate;
import java.util.List;
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
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.RoleAuthEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.news.Comment;
import studentConsulting.model.entity.news.PostEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

@Data
@Builder
@Entity
@Table(name = "conversations")
@NoArgsConstructor
@AllArgsConstructor
public class ConversationEntity{
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column( name = "id")
    private Integer id;

    @Column(name = "created_at",  updatable = false)
    private LocalDate createdAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id")
    private UserInformationEntity user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "consultant_id")
    private UserInformationEntity consultant;

    @ManyToOne
    @JoinColumn(name = "department_id",  referencedColumnName = "id")
    private DepartmentEntity department; 

    @Column(name = "name",  length = 255)
    private String name; 

    @Column(name = "status_active")
    private Boolean statusActive; 

    @Column(name = "is_group")
    private Boolean isGroup; 
    
    @OneToMany(mappedBy = "conversation", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JsonIgnore
    private List<UserInformationEntity> members;
    
 
}


