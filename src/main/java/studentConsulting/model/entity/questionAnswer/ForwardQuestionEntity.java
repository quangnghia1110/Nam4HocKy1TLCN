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
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "from_department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity fromDepartment; 

    @ManyToOne
    @JoinColumn(name = "to_department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity toDepartment; 

    @ManyToOne
    @JoinColumn(name = "question_id", nullable = false, referencedColumnName = "id")
    private QuestionEntity question; 

    @Column(name = "title", nullable = false, length = 255)
    private String title; 

    @Column(name = "status_forward", nullable = false)
    private Boolean statusForward;
}
