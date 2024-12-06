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
@Table(name = "common_question")
@NoArgsConstructor
@AllArgsConstructor
public class CommonQuestionEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "title", length = 255)
    private String title;

    @Column(name = "content", length = 900)
    private String content;

    @Column(name = "answer_title", length = 900)
    private String answerTitle;

    @Column(name = "answer_content", length = 900)
    private String answerContent;

    @Column(name = "file", length = 255)
    private String file;

    @Column(name = "file_answer", length = 255)
    private String fileAnswer;

    @Column(name = "status")
    private Boolean status;

    @ManyToOne
    @JoinColumn(name = "department_id", referencedColumnName = "id")
    private DepartmentEntity department;

    @ManyToOne
    @JoinColumn(name = "created_by", referencedColumnName = "id")
    private UserInformationEntity createdBy;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CommonQuestionEntity that = (CommonQuestionEntity) o;

        return Objects.equals(id, that.id);
    }
}
