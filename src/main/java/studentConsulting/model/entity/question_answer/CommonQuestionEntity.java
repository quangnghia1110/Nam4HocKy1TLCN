package studentConsulting.model.entity.question_answer;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.entity.user.RoleAskEntity;
import studentConsulting.model.entity.user.UserInformationEntity;

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

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserInformationEntity user;

    @ManyToOne
    @JoinColumn(name = "department_id", referencedColumnName = "id")
    private DepartmentEntity department;

    @ManyToOne
    @JoinColumn(name = "field_id", referencedColumnName = "id")
    private FieldEntity field;

    @ManyToOne
    @JoinColumn(name = "role_ask_id", referencedColumnName = "id")
    private RoleAskEntity roleAsk;

    @Column(name = "title", length = 255)
    private String title;

    @Column(name = "content", length = 900)
    private String content;

    @Column(name = "views")
    private Integer views;

    @Column(name = "file_name", length = 255)
    private String fileName;

    @Column(name = "status")
    private Integer status;

    @Column(name = "answer_title", length = 900)
    private String answerTitle;

    @Column(name = "answer_content", length = 900)
    private String answerContent;

    @Column(name = "answer_user_email", length = 255)
    private String answerUserEmail;

    @Column(name = "answer_created_at", updatable = false)
    private LocalDate answerCreatedAt;

    @Column(name = "answer_user_firstname", length = 255)
    private String answerUserFirstname;

    @Column(name = "answer_user_lastname", length = 255)
    private String answerUserLastname;

    @Column(name = "asker_firstname", length = 255)
    private String askerFirstname;

    @Column(name = "asker_lastname", length = 255)
    private String askerLastname;

    @ManyToOne
    @JoinColumn(name = "created_by")
    private UserInformationEntity createdBy;

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
