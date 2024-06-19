package studentConsulting.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@Entity
@Table(name = "fields")
@NoArgsConstructor
@AllArgsConstructor
public class fieldEntity {

    @Id
    @Column(name = "id", length = 50, nullable = false)
    private String id;

    @Column(name = "name", length = 255, nullable = false)
    private String name;

    @Column(name = "status", nullable = false)
    private boolean status = true;

    @OneToMany(mappedBy = "field", fetch = FetchType.LAZY)
    private List<questionEntity> questions;

    @OneToMany(mappedBy = "field", fetch = FetchType.LAZY)
    private List<faqEntity> faqs;
}
