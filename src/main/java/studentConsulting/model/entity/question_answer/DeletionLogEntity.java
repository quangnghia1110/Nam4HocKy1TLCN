package studentConsulting.model.entity.question_answer;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Objects;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "deletion_log")
public class DeletionLogEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @ManyToOne
    @JoinColumn(name = "question_id")
    @JsonIgnore
    private QuestionEntity question;

    @Column(nullable = false)
    private String reason;

    @Column(nullable = false)
    private String deletedBy;

    @Column(nullable = false)
    private LocalDate deletedAt;

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DeletionLogEntity that = (DeletionLogEntity) o;

        return Objects.equals(id, that.id);
    }
}
