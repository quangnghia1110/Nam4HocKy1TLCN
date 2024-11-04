package studentConsulting.model.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Objects;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "role_consultant")
@NoArgsConstructor
@AllArgsConstructor
public class RoleConsultantEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "role_id", referencedColumnName = "id")
    private RoleEntity role;

    @Column(name = "name", length = 50)
    private String name;

    @OneToMany(mappedBy = "roleConsultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AnswerEntity> answers;

    @OneToMany(mappedBy = "roleConsultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AccountEntity> accounts;

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RoleConsultantEntity that = (RoleConsultantEntity) o;

        return Objects.equals(id, that.id);
    }
}
