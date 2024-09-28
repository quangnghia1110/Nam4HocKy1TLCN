package studentConsulting.model.entity.departmentField;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "fields")
@NoArgsConstructor
@AllArgsConstructor
public class FieldEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @Column(name = "name", length = 255)
    private String name;

    @ManyToOne
    @JoinColumn(name = "department_id", referencedColumnName = "id")
    private DepartmentEntity department;

    @OneToMany(mappedBy = "field", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions;

    @OneToMany(mappedBy = "field", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<CommonQuestionEntity> commonQuestions;
}
