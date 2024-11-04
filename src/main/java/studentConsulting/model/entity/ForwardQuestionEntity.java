package studentConsulting.model.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Objects;

@Data
@Builder
@Entity
@Table(name = "forward_question")
@NoArgsConstructor
@AllArgsConstructor
public class ForwardQuestionEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "from_department_id", referencedColumnName = "id")
    private DepartmentEntity fromDepartment;

    @ManyToOne
    @JoinColumn(name = "to_department_id", referencedColumnName = "id")
    private DepartmentEntity toDepartment;

    @ManyToOne
    @JoinColumn(name = "question_id", referencedColumnName = "id")
    private QuestionEntity question;

    @Column(name = "title", length = 255)
    private String title;

    @Column(name = "status_forward")
    private Boolean statusForward;

    @ManyToOne
    @JoinColumn(name = "consultant_id", referencedColumnName = "id")
    private UserInformationEntity consultant;

    @ManyToOne
    @JoinColumn(name = "created_by", referencedColumnName = "id")
    private UserInformationEntity createdBy;

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ForwardQuestionEntity that = (ForwardQuestionEntity) o;

        return Objects.equals(id, that.id);
    }
}
