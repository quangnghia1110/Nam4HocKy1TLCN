package studentConsulting.model.entity.questionAnswer;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;

import java.sql.Timestamp;
import java.time.LocalDate;

@Data
@Builder
@Entity
@Table(name = "forward_questions")
@NoArgsConstructor
@AllArgsConstructor
public class ForwardQuestionEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column( name = "id")
    private Integer id;

    @Column(name = "created_at",  updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "from_department_id",  referencedColumnName = "id")
    private DepartmentEntity fromDepartment; 

    @ManyToOne
    @JoinColumn(name = "to_department_id",  referencedColumnName = "id")
    private DepartmentEntity toDepartment; 

    @ManyToOne
    @JoinColumn(name = "question_id",  referencedColumnName = "id")
    private QuestionEntity question; 

    @Column(name = "title",  length = 255)
    private String title; 

    @Column(name = "status_forward")
    private Boolean statusForward;
}
